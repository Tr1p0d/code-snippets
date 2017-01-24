{-# LANGUAGE TemplateHaskell #-}
module GMachine.Type.GMState
    ( GMState(GMState)
    , _gCode
    , _gDump
    , _gGlobals
    , _gHeap
    , _gOutput
    , _gStack
    , gCode
    , gDump
    , gGlobals
    , gHeap
    , gOutput
    , gStack
    , module GMachine.Type.GMDump
    , module GMachine.Type.GMOutput
    , module GMachine.Type.Globals
    , module GMachine.Type.Node
    )
  where

import Control.Lens (makeLenses)

import GMachine.Type.Address (Address)
import GMachine.Type.Globals (Globals)
import GMachine.Type.GMDump (GMDump)
import GMachine.Type.GMOutput
import GMachine.Type.Heap
import GMachine.Type.InstructionSet
import GMachine.Type.Node


data GMState = GMState
    { _gOutput :: GMOutput
    , _gCode :: GMCode
    , _gStack :: [Address]
    , _gDump :: GMDump
    , _gHeap :: Heap Node
    , _gGlobals :: Globals
    }
makeLenses ''GMState
