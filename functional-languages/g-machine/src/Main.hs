{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    (main)
  where

import qualified Data.Map.Strict as M
import Control.Lens

import Core
import Heap


data Instruction
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Slide Int
  deriving (Eq, Show)
type GMCode = [Instruction]

type Addr = Int

data Node
    = NNode Int
    | NApp Addr Addr
    | NGlobal Int GMCode
  deriving (Show)

type Globals = M.Map Name Addr

data GMState = GMState
    { _gCode :: GMCode
    , _gStack :: [Addr]
    , _gHeap :: Heap Node
    , _gGlobals :: Globals
    }
makeLenses ''GMState

instance Show GMState where
    show GMState{..} = code +\+ stack +\+ heap +\+ globals
      where
        s1 +\+ s2 = s1 ++ "\n\n" ++ s2
        code = show _gCode
        stack = show _gStack
        heap = show _gHeap
        globals = show _gGlobals

evalG :: GMState -> [GMState]
evalG state
    | isFinal = []
    | otherwise = state : (evalG $ gStep state)
  where
    isFinal = null (state ^. gCode)

gStep :: GMState -> GMState
gStep state@(GMState (i:is) _ _ _) = dispatch i (state & gCode .~ is)

dispatch :: Instruction -> GMState -> GMState
dispatch (Pushglobal n) state@GMState{..} =
    let address = _gGlobals M.! n in state & gStack %~ (address:)

dispatch (Pushint n) state@GMState{..} =
    let (nAddress, nHeap) = hAlloc _gHeap (NNode n)
    in state & gStack %~ (nAddress:) & gHeap .~ nHeap

dispatch Mkap state@(GMState _ (a1:a2:as) heap _) =
    let (nAddress, nHeap) = hAlloc heap (NApp a1 a2)
    in state { _gStack = (nAddress:as), _gHeap = nHeap }

dispatch (Slide n) state@GMState{..} =
    let (a:as) = _gStack
    in state & gStack .~ (a:drop n as)

dispatch (Push n) state@GMState{..} =
    let getArg (NApp a1 a2) = a2
        getArg n = error $ "Not an application node!" ++ show n
        a = getArg (hLookup _gHeap (_gStack !! (n + 1)))
    in state & gStack %~ (a:)

dispatch Unwind state = unwind state

unwind :: GMState -> GMState
unwind state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NNode n -> state
    NApp a1 a2 -> state & gStack %~ (a1:) & gCode .~ [Unwind]
    NGlobal nParams c ->
        if nParams < length (tail _gStack)
        then error "Not enough arguments to unwind supercombinator"
        else state & gCode .~ c

compile :: CoreProgram -> GMState
compile program = GMState initialCode [] heap globals
  where
    (heap, globals) = buildInitialHeap program
    initialCode = [Pushglobal "main", Unwind]

buildInitialHeap :: CoreProgram -> (Heap Node, Globals)
buildInitialHeap program = foldl alloc (hInitial, M.empty) compiled
  where
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
compileR e env = compileC env e ++ [Slide (length env + 1), Unwind]

compileC :: [(Name, Int)] -> CoreExpr -> GMCode
compileC env (EVar name)
    | elem name domain = [Push n]
    | otherwise = [Pushglobal name]
  where
    n = (M.fromAscList env) M.! name
    domain = M.keys $ M.fromAscList env
compileC env expr = case expr of
    ENum n -> [Pushint n]
    EAp e1 e2 -> compileC env e2 ++ compileC (argOffset 1 env) e1 ++ [Mkap]
  where
    argOffset n env' = [(v, n+m) | (v,m) <- env']

main :: IO ()
main =
    let (a:_) = reverse $ evalG $ compile testProgram1
    in print (hLookup (a ^. gHeap) (head $ a ^. gStack))

testProgram1 :: CoreProgram
testProgram1 = [("main", [], EVar "S" `EAp` (EVar "K") `EAp` (EVar "K") `EAp` (ENum 3))]

testProgram2 :: CoreProgram
testProgram2 = [("main", [], EVar "K" `EAp` ENum 4 `EAp` ENum 3)]
