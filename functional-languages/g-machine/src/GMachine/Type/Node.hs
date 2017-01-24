module GMachine.Type.Node
    (Node(..))
  where

import GMachine.Type.Address (Address)
import GMachine.Type.InstructionSet (GMCode)


data Node
    = NNode Integer
    | NApp Address Address
    | NGlobal Int GMCode
    | NInd Address
    | NConstr Integer [Address]
  deriving (Show)
