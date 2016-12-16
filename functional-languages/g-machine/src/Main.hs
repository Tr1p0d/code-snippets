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
  deriving (Show)
makeLenses ''GMState

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

dispatch (Push n) _ = error "Not implemented"

dispatch Unwind state = unwind state

unwind :: GMState -> GMState
unwind state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NNode n -> state
    NApp a1 a2 -> state & gStack %~ ((a1:) . (a2:)) & gCode .~ [Unwind]
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
    | elem name (

main :: IO ()
main = return ()























