{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module GMachine.Compiler
    (compile)
  where

import Control.Monad ((>=>), (<=<))
import qualified Data.List as L (lookup)

import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import qualified Data.Map.Strict as M

import Control.Lens ((&), (?~), at)

import GMachine.Type.Compiler
    ( AlternativesCompiler
    , Compiler
    , GCompiledSC
    , GMCompiler
    , argOffset
    , execCompiler
    , extendEnvironment
    )
import GMachine.Type.Common (Name)
import GMachine.Type.Core
import GMachine.Type.Heap (Heap, hAlloc, hInitial)
import GMachine.Type.GMState (GMState(GMState), Node(..))
import GMachine.Type.InstructionSet
    ( GMCode
    , Instruction(..)
    , ArithOp(..)
    , RelOp(..)
    )
import GMachine.Type.Globals as Glob


compile :: CoreProgram -> GMState
compile program = GMState [] initialCode [] [] heap globals
  where
    (heap, globals) = let a=a in a program
    initialCode = [Pushglobal "main", Eval, Print]
--
--buildInitialHeap :: CoreProgram -> (Heap Node, Globals)
--buildInitialHeap program =
--    foldl alloc (hInitial, Glob.empty) (compiled ++ primitives)
--  where
--    primitives =
--        [ ("+", 2, binary $ Arith Add)
--        , ("-", 2, binary $ Arith Sub)
--        , ("*", 2, binary $ Arith Mul)
--        , ("/", 2, binary $ Arith Div)
--        , ("==", 2, binary $ Rel Eq)
--        , ("if", 3,
--            [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
--        ]
--      where
--        binary op = [Push 1, Eval, Push 1, Eval, op, Update 2, Pop 2, Unwind]
--    compiled = map compileSc (program ++ preludes)
--    alloc (heap, globals) (name, nArgs, code) = (newHeap, newGlobals)
--      where
--        (a, newHeap) = hAlloc heap (NGlobal nArgs code)
--        newGlobals = globals & getGlobals.at name ?~ a
--
--compileSc :: CoreScDefn -> GCompiledSC
--compileSc (name, args, expr) =
--    (name, length args, compileR expr (zip args [0..]))

--compileR :: CoreExpr -> [(Name, Int)] -> GMCode

compileR :: GMCompiler
compileR e =
    compileC e
    updateSequence
  where
    updateSequence d = do
        d <- length <$> get
        tell [Update d, Pop d, Unwind]

compileC :: GMCompiler
compileC = \case
    ENum n -> tell [Pushint n]
    EAp e1 e2 -> do
        compileC e2
        argOffset 1
        compileC e1
        tell [Mkap]
    ECase expr' alts -> undefined --compileC env expr' ++ [CaseJump (compileAlts alts env)]
    EConstr tag arity exprs -> do
        compileCPack (reverse exprs)
        tell $ [Pack tag arity]
    EVar name -> compileCVar name
    ELet recursive defs e -> undefined -- compileCLet recursive defs e
  where
    compileCPack = mapM_ (compileC >=> const (argOffset 1))

    compileCVar name =
        get >>=
        maybe
            (tell $ [Pushglobal name])
            (tell . (:[]) . Push) . fromIntegral . L.lookup name

    compileCLet recursive defs e
        | recursive = compileLetRec compileC defs env e
        | otherwise = compileLet compileC defs env e

compileArgs defs env =
    let n = length defs
    in  (zip (map fst defs) [n-1, n-2 .. 0]) ++ argOffset n env

compileAlts :: AlternativesCompiler
compileAlts = tell . (:[]) . CaseJump <=< mapM compileAlt
  where
    compileAlt :: CoreAlt -> Compiler (Integer, GMCode)
    compileAlt (tag, names, body) = do
        env <- get
        pure (tag, execCompiler env compileAlt')
      where
        compileAlt' = do
            n <- fromIntegral $ extendEnvironment names
            tell $ [Split n]
            compileR body
            tell $ [Slide n]

compileLet :: GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLet compile' defs env expr =
    compileLet' defs env
    ++ compile' (compileArgs defs env) expr
    ++ [Slide (length defs)]
  where
    compileLet' [] _ = []
    compileLet' ((_,expr'):args) env' =
        compileC env' expr' ++ compileLet' args (argOffset 1 env')

compileLetRec :: GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLetRec compile' defs env expr =
    let n = length defs
        env' = compileArgs defs env
    in [Alloc n] ++ compileLetRec' defs env' ++ compile' env' expr ++ [Slide n]
  where
    compileLetRec' [] _ = []
    compileLetRec' ((name, expr'):args) env' =
        compileC env' expr'
        ++ [Update (length args)]
        ++ compileLetRec' args (argOffset 1 env')
