module GMachine.Type.InstructionSet
    ( GMCode
    , Instruction(..)
    , ArithOp(..)
    , RelOp(..)
    )
   where

import GMachine.Type.Common (Name)


type GMCode = [Instruction]

data Instruction
    = Alloc Int
    | CaseJump [(Integer, GMCode)]
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

