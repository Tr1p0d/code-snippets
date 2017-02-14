{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- |
-- Module      : $Header$
-- Description : The graph machine compiler
-- Copyright   : (c) Marek Kidon, 2017
-- License     : GPL-3
-- Maintainer  : marek.kidon@gmail.com
-- Stability   : experimental
-- Portability:  GHC specific language extensions.
--
-- This module contains the graph machine compiler implementation
-- using the graph machine compiler monad.
module GMachine.Compiler
    (compile)
  where

import Control.Monad ((>=>), (<=<))
import qualified Data.Map as M (lookup)

import Control.Monad.State (get, gets)
import Control.Monad.Writer (tell)

import Control.Lens ((&), (?~), at)

import GMachine.Type.Compiler
    ( AlternativesCompiler
    , CompiledProgram
    , CompiledSupercombinator(CompiledSupercombinator)
    , GMCompiler
    , LetCompiler
    , _environment
    , _scArguments
    , _scCode
    , _scName
    , envLength
    , execCompiler
    , extendEnvironment
    , extendEnvironment1
    , newEnvironment
    , offsetEnvironment
    , restoreEnvironment
    )
import GMachine.Type.Core
import GMachine.Type.Heap (Heap, hAlloc, hInitial)
import GMachine.Type.GMState (GMState(GMState), Node(..))
import GMachine.Type.InstructionSet
    ( Instruction(..)
    , ArithOp(..)
    , RelOp(..)
    )
import GMachine.Type.Globals as Glob


compile :: CoreProgram -> GMState
compile program = GMState [] initialCode [] [] heap globals
  where
    (heap, globals) = buildInitialHeap program
    initialCode = [Pushglobal "main", Eval, Print]

buildInitialHeap :: CoreProgram -> (Heap Node, Globals)
buildInitialHeap program =
    foldl alloc (hInitial, Glob.empty) (compiled ++ precompiledPrimitives)
  where
    compiled = map compileSc (program ++ prelude)

    alloc (heap, globals) CompiledSupercombinator{..} = (newHeap, newGlobals)
      where
        (a, newHeap) = hAlloc heap (NGlobal _scArguments _scCode)
        newGlobals = globals & getGlobals.at _scName ?~ a

precompiledPrimitives :: CompiledProgram
precompiledPrimitives = map (\(a,b,c) -> CompiledSupercombinator a b c)
    [ ("+", 2, binary $ Arith Add)
    , ("-", 2, binary $ Arith Sub)
    , ("*", 2, binary $ Arith Mul)
    , ("/", 2, binary $ Arith Div)
    , ("==", 2, binary $ Rel Eq)
    , ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
    ]
  where
    binary op = [Push 1, Eval, Push 1, Eval, op, Update 2, Pop 2, Unwind]

compileSc :: CoreSupercombinator -> CompiledSupercombinator
compileSc Supercombinator{..} = CompiledSupercombinator
    _scName
    (length _scArguments)
    (execCompiler (newEnvironment _scArguments) $ compileR _scBody)

compileR :: GMCompiler
compileR e = do
    compileC e
    updateSequence
  where
    updateSequence = do
        d <- envLength <$> get
        tell [Update d, Pop d, Unwind]

compileC :: GMCompiler
compileC = \case
    ENum n -> tell [Pushint n]
    EAp e1 e2 -> do
        compileC e2
        restoreEnvironment $ do
            offsetEnvironment 1
            compileC e1
        tell [Mkap]
    ECase expr' alts -> do
        compileC expr'
        compileAlts alts
    EConstr tag arity exprs -> do
        compileCPack (reverse exprs)
        tell [Pack tag arity]
    EVar name -> compileCVar name
    ELet recursive defs e -> compileCLet recursive defs e
    -- | The anonymous functions (lambdas) are lambda-lifted and
    -- given a random name.
  where
    compileCPack = mapM_ (compileC >=> const (offsetEnvironment 1))

    compileCVar name =
        gets _environment >>=
        tell . maybe
            [Pushglobal name]
            ((:[]) . Push . fromIntegral) . M.lookup name

    compileCLet recursive defs e
        | recursive = restoreEnvironment $ compileLetRec defs e
        | otherwise = restoreEnvironment $ compileLet defs e

compileAlts :: AlternativesCompiler
compileAlts = tell . (:[]) . CaseJump <=< mapM compileAlt
  where
    compileAlt Alternative{..} = do
        env <- get
        pure (_tag, execCompiler env compileAlt')
      where
        compileAlt' = do
            n <- fromIntegral <$> extendEnvironment _altArguments
            tell [Split n]
            compileR _altBody
            tell [Slide n]

compileLet :: LetCompiler
compileLet defs expr = do
    mapM_ compileLet' defs
    n <- fromIntegral <$> extendEnvironment1 (map _ldName defs)
    compileC expr
    tell [Slide n]
  where
    compileLet' LocalDefinition{..} = do
        compileC _ldBody
        offsetEnvironment 1

compileLetRec :: LetCompiler
compileLetRec defs expr = do
    let n = length defs
    tell [Alloc n]
    mapM_ compileLetRec' (zip defs [n-1 .. 0])
    compileC expr
    tell [Slide n]
  where
    compileLetRec' (LocalDefinition{..}, n) = do
        compileC _ldBody
        tell [Update n]
        offsetEnvironment 1
