{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GMachine
    (evalG)
  where

import qualified Data.Map.Strict as M

import Control.Lens

import GMachine.Type.Address (nullAddr)
import GMachine.Type.Globals as Glob
import GMachine.Type.GMState
import GMachine.Type.Heap
import GMachine.Type.InstructionSet
    ( Instruction(..)
    , ArithOp(..)
    , RelOp(..)
    )


evalG :: GMState -> [GMState]
evalG state
    | isFinal = [state]
    | otherwise = state : (evalG $ gStep state)
  where
    isFinal = null (state ^. gCode)

gStep :: GMState -> GMState
gStep (GMState _ [] _ _ _ _) = error $ "We have run out of instructions"
gStep state@(GMState _ (i:is) _ _ _ _) = dispatch i (state & gCode .~ is)

dispatch :: Instruction -> GMState -> GMState
dispatch (Pushglobal n) state@GMState{..} =
    let (Just address) = state ^. gGlobals.getGlobals.at n
    in state & gStack %~ (address:)

dispatch (Slide n) state = state & gStack %~ (drop n)

dispatch (Pushint n) state@GMState{..} =
    let (nAddress, nHeap) = hAlloc _gHeap (NNode n)
    in state & gStack %~ (nAddress:) & gHeap .~ nHeap

dispatch Mkap state@(GMState _ _ (a1:a2:as) _ heap _) =
    let (nAddress, nHeap) = hAlloc heap (NApp a1 a2)
    in state { _gStack = (nAddress:as), _gHeap = nHeap }
dispatch Mkap (GMState _ _ (_) _ _ _) = error $
    "not enough arguments to apply"

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
    allocNodes n' heap =
        let (addr, heap') = hAlloc heap (NInd nullAddr)
            (heap'', addrs) = allocNodes (n'-1) heap'
        in (heap'', addr:addrs)

dispatch Eval state@GMState{..} = state
    & gCode .~ [Unwind]
    & gStack %~ ((:[]) . head)
    & gDump %~ ((_gCode, tail _gStack):)

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

dispatch (Pack t n) state@GMState{..} =
    let n' = fromInteger n
        (newAddr, newHeap) = hAlloc _gHeap (NConstr t (take n' _gStack))
    in state & gStack %~ ((newAddr:) . drop n') & gHeap .~ newHeap

dispatch (CaseJump tagAssoc) state@GMState{..} =
    case hLookup _gHeap (head _gStack) of
        NConstr t _ -> state
            & gCode %~ ((tagMap M.! (fromInteger t)) ++)
        _ -> error "cannot multiway jump on non-constructor"
  where
    tagMap = M.fromAscList tagAssoc

-- | We ommit the number of splits since we get it from constructor itself.
dispatch (Split _) state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NConstr _ addrs -> state & gStack %~ ((addrs ++) . tail)
    _ -> error "cannot split on non-constructor"

dispatch Print state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NNode n -> state & gOutput %~ (++ number n)
    NConstr tag addrs -> state
        & gCode %~ ((concat $ replicate (length addrs) [Eval, Print]) ++)
        & gStack %~ ((addrs ++) . tail)
        & gOutput %~ (++ "Constructor: " ++ show tag ++ " ")
  where
    number num = ("Number: " ++ show num ++ " ")

dispatch (Arith binop) state@GMState{..}
    | (isNumberNode $ hLookup _gHeap (head _gStack)) &&
      (isNumberNode $ hLookup _gHeap (head $ tail _gStack)) =
        let (NNode x) = hLookup _gHeap (head _gStack)
            (NNode y) = hLookup _gHeap (head $ tail _gStack)
            (nAddress, nHeap) = case binop of
                Add -> hAlloc _gHeap $ NNode (x+y)
                Sub -> hAlloc _gHeap $ NNode (x-y)
                Mul -> hAlloc _gHeap $ NNode (x*y)
                Div -> hAlloc _gHeap $ NNode (x `div` y)
        in state & gStack %~ ((nAddress:) . drop 2) & gHeap .~ nHeap
    | otherwise = error "dyadic operation not applied to numbers"

dispatch (Rel relop) state@GMState{..}
    | (isNumberNode $ hLookup _gHeap (head _gStack)) &&
      (isNumberNode $ hLookup _gHeap (head $ tail _gStack)) =
        let (NNode x) = hLookup _gHeap (head _gStack)
            (NNode y) = hLookup _gHeap (head $ tail _gStack)
            (nAddress, nHeap) = case relop of
                Eq -> hAlloc _gHeap $ NNode $ mapInteger (x==y)
                Neq -> hAlloc _gHeap $ NNode $ mapInteger (x/=y)
                Greater -> hAlloc _gHeap $ NNode $ mapInteger (x>y)
                Geq -> hAlloc _gHeap $ NNode $ mapInteger (x>=y)
                Less -> hAlloc _gHeap $ NNode $ mapInteger (x<y)
                Leq -> hAlloc _gHeap $ NNode $ mapInteger (x<=y)
        in state & gStack %~ ((nAddress:) . drop 2) & gHeap .~ nHeap
    | otherwise = error "dyadic operation not applied to numbers"
  where
    mapInteger x
        | x = 1
        | otherwise = 0

isNumberNode :: Node -> Bool
isNumberNode (NNode _) = True
isNumberNode _ = False

unwind :: GMState -> GMState
unwind state@GMState{..} = case hLookup _gHeap (head _gStack) of
    NInd a -> state & gStack.ix 0 .~ a  & gCode .~ [Unwind]
    NConstr _ _ -> handleConstrNode
      where
        handleConstrNode
            | null $ state ^. gDump = error "dump should not be empty"
            | otherwise =
                let (instrs, stack) = head _gDump
                in state
                    & gCode .~ instrs
                    & gStack._tail .~ stack
                    & gDump %~ tail
    NNode _ -> handleNumberNode
      where
        handleNumberNode
            | null $ state ^. gDump = state
            | otherwise =
                let (instrs, stack) = head _gDump
                in state
                    & gCode .~ instrs
                    & gStack._tail .~ stack
                    & gDump %~ tail
    NApp a1 _ -> state & gStack %~ (a1:) & gCode .~ [Unwind]
    NGlobal nParams c ->
        if nParams > length (tail _gStack)
        then error $ "Not enough arguments to unwind"
        else state & gCode .~ c & gStack %~ rearrangeStack
      where
        rearrangeStack stack = take nParams newStack ++ drop nParams stack
          where
            getArg (NApp _ a2) = a2
            getArg n = error $ "Not an application node!" ++ show n
            newStack = map (getArg . hLookup _gHeap) (tail stack)
