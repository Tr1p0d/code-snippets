{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : $Header$
-- Description : The graph machine compiler monad
-- Copyright   : (c) Marek Kidon, 2017
-- License     : GPL-3
-- Maintainer  : marek.kidon@gmail.com
-- Stability   : experimental
-- Portability:  GHC specific language extensions.
--
-- This module contains the graph machine compiler monad
-- with some state modifying functions and type aliases.
module GMachine.Type.Compiler
    ( AlternativesCompiler
    , CompiledProgram
    , CompiledSupercombinator(CompiledSupercombinator)
    , Compiler
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
    , extendEnvironment1_
    , extendEnvironment_
    , newEnvironment
    , offsetEnvironment
    , restoreEnvironment
    , scArguments
    , scCode
    , scName
    )
  where

import Control.Monad (void)
import Data.Int (Int)
import Data.Word (Word32)

import qualified Data.Map as M (Map, fromList, map, union)

import Control.Lens ((%~), makeLenses)

import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (Writer)
import Control.Monad.State (MonadState, execStateT, get, modify, put)
import Control.Monad.Writer (MonadWriter, execWriter)

import GMachine.Type.Common (Name)
import GMachine.Type.Core (CoreAlternatives, CoreExpr, CoreLocalDefinitions)
import GMachine.Type.InstructionSet (GMCode)


data CompiledSupercombinator = CompiledSupercombinator
    { _scName :: Name
    , _scArguments :: Int
    , _scCode :: GMCode
    }
makeLenses ''CompiledSupercombinator

type CompiledProgram = [CompiledSupercombinator]

newtype Environment = Environment { _environment :: M.Map Name Word32 }
  deriving (Show)
makeLenses ''Environment

type AlternativesCompiler = CoreAlternatives -> Compiler ()

type LetCompiler = CoreLocalDefinitions -> GMCompiler

-- | The GMachine compiler type alias
type GMCompiler = CoreExpr -> Compiler ()

-- | Monad that holds the Core Expression and Argument offsets.
newtype Compiler a =
    Compiler { getCompiler :: StateT Environment (Writer GMCode) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Environment
    , MonadWriter GMCode
    )


newEnvironment :: [Name] -> Environment
newEnvironment =  Environment . M.fromList . flip zip [0..]

envLength :: Environment -> Int
envLength = length . _environment

offsetEnvironment :: Word32 -> Compiler ()
offsetEnvironment n = modify (environment %~ M.map (+n))

restoreEnvironment :: Compiler a -> Compiler a
restoreEnvironment compile = do
    s <- get
    a <- compile
    put s
    pure a

extendEnvironment :: [Name] -> Compiler Word32
extendEnvironment names = do
    let n = fromIntegral $ length names
    offsetEnvironment n
    modify (environment %~ (M.fromList (zip names [0..]) `M.union`))
    pure n

extendEnvironment_ :: [Name] -> Compiler ()
extendEnvironment_ = void . extendEnvironment

extendEnvironment1 :: [Name] -> Compiler Word32
extendEnvironment1 names = do
    let n = fromIntegral $ length names
    offsetEnvironment n
    modify (environment %~ (M.fromList (zip names [n-1, n-2 .. 0]) `M.union`))
    pure n

extendEnvironment1_ :: [Name] -> Compiler ()
extendEnvironment1_ = void . extendEnvironment1_

execCompiler :: Environment -> Compiler () -> GMCode
execCompiler env = execWriter . flip execStateT env . getCompiler
