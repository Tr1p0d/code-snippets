{-# LANGUAGE TemplateHaskell #-}
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

data GMState = GMState
    { _gCode :: GMCode
    , _gStack :: [Addr]
    , _gHeap :: Heap Node
    , _gGlobals :: M.Map Name Addr
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

main :: IO ()
main = return ()
