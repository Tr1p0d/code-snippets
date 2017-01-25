module GMachine.Type.Node
    (Node(..))
  where

import GMachine.Type.Address (Address)
import GMachine.Type.InstructionSet (GMCode)

import Text.PrettyPrint (text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))


data Node
    = NNode Integer
    | NApp Address Address
    | NGlobal Int GMCode
    | NInd Address
    | NConstr Integer [Address]
  deriving (Show)

instance Pretty Node where
    pPrint node = text $ show node
