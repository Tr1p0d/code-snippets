module GMachine.Compiler
    (compile)
  where

import qualified Data.Map.Strict as M

import Control.Lens ((&), (?~), at)

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
    (heap, globals) = buildInitialHeap program
    initialCode = [Pushglobal "main", Eval, Print]

buildInitialHeap :: CoreProgram -> (Heap Node, Globals)
buildInitialHeap program = foldl alloc (hInitial, Glob.empty) (compiled ++ primitives)
  where
    primitives =
        [ ("+", 2, [Push 1, Eval, Push 1, Eval, Arith Add, Update 2, Pop 2, Unwind])
        , ("-", 2, [Push 1, Eval, Push 1, Eval, Arith Sub, Update 2, Pop 2, Unwind])
        , ("*", 2, [Push 1, Eval, Push 1, Eval, Arith Mul, Update 2, Pop 2, Unwind])
        , ("/", 2, [Push 1, Eval, Push 1, Eval, Arith Div, Update 2, Pop 2, Unwind])
        , ("==", 2, [Push 1, Eval, Push 1, Eval, Rel Eq, Update 2, Pop 2, Unwind])
        , ("if", 3,
            [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
        ]
    compiled = map compileSc (program ++ preludes)
    alloc (heap, globals) (name, nArgs, code) = (newHeap, newGlobals)
      where
        (a, newHeap) = hAlloc heap (NGlobal nArgs code)
        newGlobals = globals & getGlobals.at name ?~ a

type GCompiledSC = (Name, Int, GMCode)

compileSc :: CoreScDefn -> GCompiledSC
compileSc (name, args, expr) =
    (name, length args, compileR expr (zip args [0..]))

compileR :: CoreExpr -> [(Name, Int)] -> GMCode
compileR e env =
    let d = length env
    in compileC env e ++ [Update d, Pop d, Unwind]

type GMCompiler = [(Name, Int)] -> CoreExpr -> GMCode

argOffset :: Num b => b -> [(a, b)] -> [(a, b)]
argOffset n env' = [(v, n+m) | (v,m) <- env']

compileC :: [(Name, Int)] -> Expr Name -> [Instruction]
compileC env expr = case expr of
    ENum n -> [Pushint n]
    EAp e1 e2 -> compileC env e2 ++ compileC (argOffset 1 env) e1 ++ [Mkap]
    ECase expr' alts -> compileC env expr' ++ [CaseJump (compileAlts alts env)]
    EConstr tag arity exprs ->
        compilePack env (reverse exprs) ++ [Pack tag arity]
    EVar name -> compileCVar name
    ELet recursive defs e -> compileCLet recursive defs e
    -- I wonder how ELam is compiled...
  where
    compilePack _ [] = []
    compilePack env' (expr':exprs') =
        compileC env' expr' ++ compilePack (argOffset 1 env') exprs'

    compileCVar name
        | name `M.member` asc = [Push $ asc M.! name]
        | otherwise = [Pushglobal name]
      where
        asc = M.fromAscList env

    compileCLet recursive defs e
        | recursive = compileLetRec compileC defs env e
        | otherwise = compileLet compileC defs env e

compileArgs defs env =
    let n = length defs
    in  (zip (map fst defs) [n-1, n-2 .. 0]) ++ argOffset n env

compileAlts :: [CoreAlt] -> [(Name, Int)] -> [(Integer, GMCode)]
compileAlts alts env = map compileAlt alts
  where
    compileAlt (tag, names, body) =
        let n = length names
            newEnv = zip names [0..] ++ argOffset n env
        in (tag, [Split n] ++ compileR body newEnv ++ [Slide n])

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
