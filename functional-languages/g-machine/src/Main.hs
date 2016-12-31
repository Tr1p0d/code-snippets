{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    (main)
  where

import qualified Data.Map.Strict as M
import Control.Lens
import Control.Lens.Cons

import Core
import Heap

import Debug.Trace

data Instruction
    = Add
    | Alloc Int
    | Cond GMCode GMCode
    | Eq
    | Eval
    | Mkap
    | Pop Int
    | Push Int
    | Pushglobal Name
    | Pushint Int
    | Slide Int
    | Unwind
    | Update Int
  deriving (Eq, Show)
type GMCode = [Instruction]

type Addr = Int

data Node
    = NNode Int
    | NApp Addr Addr
    | NGlobal Int GMCode
    | NInd Addr
  deriving (Show)

type Globals = M.Map Name Addr

type GMDump = [(GMCode, [Addr])]

data GMState = GMState
    { _gCode :: GMCode
    , _gStack :: [Addr]
    , _gDump :: GMDump
    , _gHeap :: Heap Node
    , _gGlobals :: Globals
    }
makeLenses ''GMState

instance Show GMState where
    show GMState{..} = code +\+ stack +\+ dump +\+ heap +\+ globals
      where
        s1 +\+ s2 = s1 ++ "\n\n" ++ s2
        code = show _gCode
        stack = show _gStack
        heap = show _gHeap
        globals = show _gGlobals
        dump = show _gDump

evalG :: GMState -> [GMState]
evalG state
    | isFinal = []
    | otherwise = state : (evalG $ gStep state)
  where
    isFinal = null (state ^. gCode)

gStep :: GMState -> GMState
gStep state@(GMState (i:is) _ _ _ _) = dispatch i (state & gCode .~ is)

dispatch :: Instruction -> GMState -> GMState
dispatch (Pushglobal n) state@GMState{..} =
    let address = _gGlobals M.! n in state & gStack %~ (address:)

dispatch (Slide n) state = state & gStack %~ (drop n)

dispatch (Pushint n) state@GMState{..} =
    let (nAddress, nHeap) = hAlloc _gHeap (NNode n)
    in state & gStack %~ (nAddress:) & gHeap .~ nHeap

dispatch Mkap state@(GMState _ (a1:a2:as) _ heap _) =
    let (nAddress, nHeap) = hAlloc heap (NApp a1 a2)
    in state { _gStack = (nAddress:as), _gHeap = nHeap }

dispatch (Pop n) state = state & gStack %~ (drop n)

dispatch (Push n) state@GMState{..} = state & gStack %~ (_gStack !! n:)

dispatch (Update n) state@GMState{..} =
    let indNode = NInd $ head $ state ^. gStack
        newStack = tail _gStack
        (Just atAddr) = state ^? gStack._tail.ix n
    in state & gHeap %~ hSetAt atAddr indNode & gStack .~ newStack

dispatch Unwind state = unwind state

dispatch (Alloc n) state@GMState{..} =
    let (newHeap, addrs) = allocNodes n _gHeap
    in state & gHeap .~ newHeap & gStack %~ (addrs++)
  where
    allocNodes 0 heap = (heap, [])
    allocNodes n heap =
        let (addr, heap') = hAlloc heap (NInd hNull)
            (heap'', addrs) = allocNodes (n-1) heap'
        in (heap'', addr:addrs)

dispatch Eval state@GMState{..} = state
    & gCode .~ [Unwind]
    & gStack %~ ((:[]) . head)
    & gDump %~ ((_gCode, tail _gStack):)

dispatch Add state@GMState{..}
    | (isNumberNode $ hLookup _gHeap (head _gStack)) &&
      (isNumberNode $ hLookup _gHeap (head $ tail _gStack)) =
        let (nAddress, nHeap) = hAlloc _gHeap (NNode (x+y))
            (NNode x) = hLookup _gHeap (head _gStack)
            (NNode y) = hLookup _gHeap (head $ tail _gStack)
        in state & gStack %~ ((nAddress:) . drop 2) & gHeap .~ nHeap
    | otherwise = error "dyadic operation not applied to numbers"

dispatch Eq state@GMState{..}
    | (isNumberNode $ hLookup _gHeap (head _gStack)) &&
      (isNumberNode $ hLookup _gHeap (head $ tail _gStack)) =
        let (nAddress, nHeap) = hAlloc _gHeap
                (NNode (mapInteger (x == y)))
            (NNode x) = hLookup _gHeap (head _gStack)
            (NNode y) = hLookup _gHeap (head $ tail _gStack)
        in state & gStack %~ ((nAddress:) . drop 2) & gHeap .~ nHeap
    | otherwise = error "dyadic operation not applied to numbers"
  where
    mapInteger x
        | x = 1
        | otherwise = 0

dispatch (Cond i1 i2) state@GMState{..}
    | isNumberNode $ hLookup _gHeap (head _gStack) =
        if mapBoolean $ hLookup _gHeap (head _gStack)
        then state & gCode %~ (i1 ++)
        else state & gCode %~ (i2 ++)
    | otherwise = error "condition operation is not a number"
  where
    mapBoolean (NNode x)
        | x == 1 = True
        | x == 0 = False
        | otherwise = error "not a boolean"

isNumberNode (NNode _) = True
isNumberNode _ = False


hNull :: Addr
hNull = error $ "invalid heap address"

unwind :: GMState -> GMState
unwind state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NInd a -> state & gStack.ix 0 .~ a  & gCode .~ [Unwind]
    NNode n -> handleNumberNode
      where
        handleNumberNode
            | null $ state ^. gDump = state
            | otherwise =
                let (instrs, stack) = head _gDump
                in state
                    & gCode .~ instrs
                    & gStack._tail .~ stack
                    & gDump %~ tail
    NApp a1 a2 -> state & gStack %~ (a1:) & gCode .~ [Unwind]
    NGlobal nParams c ->
        if nParams > length (tail _gStack)
        then error $ "Not enough arguments to unwind"
        else state & gCode .~ c & gStack %~ rearrangeStack
      where
        rearrangeStack stack = take nParams newStack ++ drop nParams stack
          where
            getArg (NApp a1 a2) = a2
            getArg n = error $ "Not an application node!" ++ show n
            newStack = map (getArg . hLookup _gHeap) (tail stack)

compile :: CoreProgram -> GMState
compile program = GMState initialCode [] [] heap globals
  where
    (heap, globals) = buildInitialHeap program
    initialCode = [Pushglobal "main", Eval]

buildInitialHeap :: CoreProgram -> (Heap Node, Globals)
buildInitialHeap program = foldl alloc (hInitial, M.empty) (compiled ++ primitives)
  where
    primitives =
        [ ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
        --, ("-" 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
        --, ("*" 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
        --, ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
        , ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
        , ("if", 3,
            [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
        ]
    compiled = map compileSc (program ++ preludes)
    alloc (heap, globals) (name, nArgs, code) = (newHeap, newGlobals)
      where
        (a, newHeap) = hAlloc heap (NGlobal nArgs code)
        newGlobals = M.insert name a globals

type GCompiledSC = (Name, Int, GMCode)

compileSc :: CoreScDefn -> GCompiledSC
compileSc (name, args, expr) =
    (name, length args, compileR expr (zip args [0..]))

compileR :: CoreExpr -> [(Name, Int)] -> GMCode
compileR e env =
    let d = length env
    in compileC env e ++ [Update d, Pop d, Unwind]

type GMCompiler = [(Name, Int)] -> CoreExpr -> GMCode

argOffset n env' = [(v, n+m) | (v,m) <- env']

compileC :: GMCompiler
compileC env (EVar name)
    | name `M.member` asc = [Push n]
    | otherwise = [Pushglobal name]
  where
    asc = M.fromAscList env
    n = asc M.! name
compileC env (ELet recursive defs e)
    | recursive = compileLetRec compileC defs env e
    | otherwise = compileLet compileC defs env e
compileC env expr = case expr of
    ENum n -> [Pushint n]
    EAp e1 e2 -> compileC env e2 ++ compileC (argOffset 1 env) e1 ++ [Mkap]

compileArgs defs env =
    let n = length defs
    in  (zip (map fst defs) [n-1, n-2 .. 0]) ++ argOffset n env

compileLet :: GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLet compile' defs env expr =
    compileLet' defs env
    ++ compile' (compileArgs defs env) expr
    ++ [Slide (length defs)]
  where
    compileLet' [] env = []
    compileLet' ((name,expr):args) env =
        compileC env expr ++ compileLet' args (argOffset 1 env)

compileLetRec :: GMCompiler -> [(Name, CoreExpr)] -> GMCompiler
compileLetRec compile' defs env expr =
    let n = length defs
        env' = compileArgs defs env
    in [Alloc n] ++ compileLetRec' defs env' ++ compile' env' expr ++ [Slide n]
  where
    compileLetRec' [] env' = []
    compileLetRec' ((name, expr'):args) env' =
        compileC env' expr'
        ++ [Update (length args)]
        ++ compileLetRec' args (argOffset 1 env')

main :: IO ()
main =
    let (a:_) = reverse $ evalG $ compile testProgram1
    in print (hLookup (a ^. gHeap) (head $ a ^. gStack))

testProgram1 :: CoreProgram
testProgram1 =
    [("main", [], EVar "S" `EAp` (EVar "K") `EAp` (EVar "K") `EAp` (ENum 3))]

testProgram2 :: CoreProgram
testProgram2 = [("main", [], EVar "K" `EAp` ENum 4 `EAp` ENum 3)]

testProgram3 :: CoreProgram
testProgram3 =
    [ ("id", [], EVar "S" `EAp` EVar "K" `EAp` EVar "K")
    , ("main", [], EVar "twice" `EAp` EVar "id" `EAp` ENum 3)]

testProgram4 :: CoreProgram
testProgram4 =
    [ ("three", [], ELet False [("x", ENum 4)] (EVar "x"))
    , ("main", [], EVar "three")
    ]

testProgram5 :: CoreProgram
testProgram5 =
    [ ("main", [], EVar "+" `EAp` ENum 1 `EAp` ENum 1)
    ]
