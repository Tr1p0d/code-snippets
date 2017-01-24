module GMachine.Type.Globals
    (Globals)
  where

import qualified Data.Map as M (Map)

import GMachine.Type.Common (Name)
import GMachine.Type.Address (Address)


type Globals = M.Map Name Address
