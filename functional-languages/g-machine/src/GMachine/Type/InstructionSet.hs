module GMachine.Type.InstructionSet
    (GMCode, Instruction(..))
   where

import GMachine.Type.Common (Name)


type GMCode = [Instruction]

data Instruction
    = Add
    | Alloc Int
    | CaseJump [(Integer, GMCode)]
    | Cond GMCode GMCode
    | Eq
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
  deriving (Eq, Show)
