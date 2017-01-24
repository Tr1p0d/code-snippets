module GMachine.Type.GMDump
    (GMDump)
  where

import GMachine.Type.InstructionSet (GMCode)
import GMachine.Type.Address (Address)


type GMDump = [(GMCode, [Address])]
