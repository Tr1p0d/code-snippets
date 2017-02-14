{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : $Header$
-- Description : The graph machine
-- Copyright   : (c) Marek Kidon, 2017
-- License     : GPL-3
-- Maintainer  : marek.kidon@gmail.com
-- Stability   : experimental
-- Portability:  GHC specific language extensions.
--
-- This module contains simple transition system describing the graph
-- machine using the GMachine monad.
module GMachine
    (evalG)
  where

import qualified Data.Map.Strict as M

import Control.Lens

import GMachine.Type.Address (nullAddr)
import GMachine.Type.Globals as Glob
import GMachine.Type.GMState
import GMachine.Type.GMachine
    ( GMachineState
    , GMachineTransition
    , GMachineError
        ( NotEnoughArguments
        , OutOfInstructions
        )
    , badTransition
    )
import GMachine.Type.Heap
import GMachine.Type.InstructionSet
    ( Instruction(..)
    , ArithOp(..)
    , RelOp(..)
    )


evalG :: GMState -> [GMState]
evalG state
    | isFinal = [state]
    | otherwise = undefined --state : (evalG $ gStep state)
  where
    isFinal = null (state ^. gCode)

wind :: GMachineState
wind  = \case
    (GMState _ [] _ _ _ _) -> badTransition OutOfInstructions
    state@(GMState _ (i:is) _ _ _ _) -> windTransition (state & gCode .~ is) i

windTransition :: GMachineTransition
windTransition state@GMState{..} i = case i of
    Pushglobal n ->
        let (Just address) = state ^. gGlobals.getGlobals.at n
        in wind $ state & gStack %~ (address:)

    Slide n -> wind $ state & gStack %~ (drop n)

    Pushint n ->
        let (nAddress, nHeap) = hAlloc _gHeap (NNode n)
        in wind $ state & gStack %~ (nAddress:) & gHeap .~ nHeap

    Mkap -> case _gStack of
        (a1:a2:_) ->
            let (nAddress, nHeap) = hAlloc _gHeap (NApp a1 a2)
            in wind $ state & gStack %~ (nAddress:) & gHeap .~ nHeap
        _ -> badTransition NotEnoughArguments

    Pop n -> wind $ state & gStack %~ (drop n)

    Push n -> wind $ state & gStack %~ (_gStack !! n:)

    Update n ->
        let indNode = NInd $ head $ state ^. gStack
            newStack = state ^. gStack._tail
            (Just atAddr) = state ^? gStack._tail.ix n
        in wind $ state & gHeap %~ hSetAt atAddr indNode & gStack .~ newStack

    Alloc n ->
        let (newHeap, addrs) = allocNodes n _gHeap
        in wind $ state & gHeap .~ newHeap & gStack %~ (addrs++)
      where
        allocNodes 0 heap = (heap, [])
        allocNodes n' heap =
            let (addr, heap') = hAlloc heap (NInd nullAddr)
                (heap'', addrs) = allocNodes (n'-1) heap'
            in (heap'', addr:addrs)

    Eval -> wind $ state
        & gCode .~ [Unwind]
        & gStack %~ ((:[]) . head)
        & gDump %~ ((_gCode, tail _gStack):)

    Cond i1 i2 -> do
        boolean <- mapBoolean $ hLookup _gHeap (head _gStack)
        wind $ if boolean
            then (state & gCode %~ (i1 ++))
            else (state & gCode %~ (i2 ++))
      where
        mapBoolean (NNode x)
            | x == 1 = pure True
            | x == 0 = pure False
            | otherwise = error "not a boolean"

    Pack t n ->
        let n' = fromInteger n
            (newAddr, newHeap) = hAlloc _gHeap (NConstr t (take n' _gStack))
        in wind $ state & gStack %~ ((newAddr:) . drop n') & gHeap .~ newHeap

    CaseJump tagAssoc ->
        wind $ case hLookup _gHeap (head _gStack) of
            NConstr t _ -> state
                & gCode %~ ((tagMap M.! (fromInteger t)) ++)
            _ -> error "cannot multiway jump on non-constructor"
      where
        tagMap = M.fromAscList tagAssoc

    -- | We ommit the number of splits since we get it from constructor itself.
    Split _ -> wind $ case hLookup _gHeap (head _gStack) of
        NConstr _ addrs -> state & gStack %~ ((addrs ++) . tail)
        _ -> error "cannot split on non-constructor"

    Print -> wind $ case hLookup _gHeap (head _gStack) of
        NNode n -> state & gOutput %~ (++ number n)
        NConstr tag addrs -> state
            & gCode %~ ((concat $ replicate (length addrs) [Eval, Print]) ++)
            & gStack %~ ((addrs ++) . tail)
            & gOutput %~ (++ "Constructor: " ++ show tag ++ " ")
      where
        number num = ("Number: " ++ show num ++ " ")

    Arith binop ->
            -- | Make hLookup part of our monad
            let (NNode x) = hLookup _gHeap (head _gStack)
                (NNode y) = hLookup _gHeap (head $ tail _gStack)
                (nAddress, nHeap) = case binop of
                    Add -> hAlloc _gHeap $ NNode (x+y)
                    Sub -> hAlloc _gHeap $ NNode (x-y)
                    Mul -> hAlloc _gHeap $ NNode (x*y)
                    Div -> hAlloc _gHeap $ NNode (x `div` y)
            in wind $ state & gStack %~ ((nAddress:) . drop 2) & gHeap .~ nHeap

    Rel relop ->
        let (NNode x) = hLookup _gHeap (head _gStack)
            (NNode y) = hLookup _gHeap (head $ tail _gStack)
            (nAddress, nHeap) = case relop of
                Eq -> hAlloc _gHeap $ NNode $ mapInteger (x==y)
                Neq -> hAlloc _gHeap $ NNode $ mapInteger (x/=y)
                Greater -> hAlloc _gHeap $ NNode $ mapInteger (x>y)
                Geq -> hAlloc _gHeap $ NNode $ mapInteger (x>=y)
                Less -> hAlloc _gHeap $ NNode $ mapInteger (x<y)
                Leq -> hAlloc _gHeap $ NNode $ mapInteger (x<=y)
        in wind $ state & gStack %~ ((nAddress:) . drop 2) & gHeap .~ nHeap
      where
        mapInteger x
            | x = 1
            | otherwise = 0

    Unwind ->  case hLookup _gHeap (head _gStack) of
        NInd a -> wind $ state & gStack.ix 0 .~ a  & gCode .~ [Unwind]

        NConstr _ _ -> wind handleConstrNode
          where
            handleConstrNode
                | null $ state ^. gDump = error "dump should not be empty"
                | otherwise =
                    let (instrs, stack) = head _gDump
                    in state
                        & gCode .~ instrs
                        & gStack._tail .~ stack
                        & gDump %~ tail

        NNode _ -> wind handleNumberNode
          where
            handleNumberNode
                | null $ state ^. gDump = state
                | otherwise =
                    let (instrs, stack) = head _gDump
                    in state
                        & gCode .~ instrs
                        & gStack._tail .~ stack
                        & gDump %~ tail

        NApp a1 _ -> wind $ state & gStack %~ (a1:) & gCode .~ [Unwind]

        NGlobal nParams c ->
            if nParams > length (tail _gStack)
            then error $ "Not enough arguments to unwind"
            else wind $ state & gCode .~ c & gStack %~ rearrangeStack
          where
            rearrangeStack stack = take nParams newStack ++ drop nParams stack
              where
                getArg (NApp _ a2) = a2
                getArg n = error $ "Not an application node!" ++ show n
                newStack = map (getArg . hLookup _gHeap) (tail stack)

