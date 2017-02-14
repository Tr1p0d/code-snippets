-- |
-- Module      : $Header$
-- Description : The graph machine instruction set
-- Copyright   : (c) Marek Kidon, 2017
-- License     : GPL-3
-- Maintainer  : marek.kidon@gmail.com
-- Stability   : experimental
-- Portability:  GHC specific language extensions.
--
-- This module contains the graph machine instruction set.
module GMachine.Type.InstructionSet
    ( GMCode
    , Instruction(..)
    , ArithOp(..)
    , RelOp(..)
    )
   where

import Data.Word (Word32)

import Text.PrettyPrint (text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

import GMachine.Type.Common (Name)


type GMCode = [Instruction]

data Instruction
    = Alloc Int
    | CaseJump [(Word32, GMCode)]
    | Cond GMCode GMCode
    | Eval
    | Mkap
    | Pack Integer Integer
    | Pop Int
    | Print
    | Push Int
    | Pushglobal Name
    | Pushint Integer
    | Slide Int
    | Split Int
    | Unwind
    | Update Int
    | Arith ArithOp
    | Rel RelOp
  deriving (Eq, Show)

data ArithOp
    = Add
    | Div
    | Mul
    | Sub
  deriving (Eq, Show)

data RelOp
    = Eq
    | Neq
    | Greater
    | Geq
    | Less
    | Leq
  deriving (Eq, Show)

instance Pretty Instruction where
    pPrint = text . show
