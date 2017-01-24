module GMachine.Type.Address
    (Address(..))
  where

import Data.Word (Word32)
import Text.Show (Show)


data Address
    = Null
    | Addr Word32
  deriving (Show)

hNull :: Address
hNull = Null
