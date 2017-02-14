{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module GMachine.Type.GMState
    ( GMState(GMState)
    , GMRun
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

import Text.PrettyPrint (text, vcat, fsep, nest)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))


data GMState = GMState
    { _gOutput :: GMOutput
    , _gCode :: GMCode
    , _gStack :: [Address]
    , _gDump :: GMDump
    , _gHeap :: Heap Node
    , _gGlobals :: Globals
    }
makeLenses ''GMState

type GMRun = [GMState]

instance Pretty GMState where
    pPrint GMState{..} = vcat
        [ output
        , code
        , stack
        , dump
        , heap
        , globals
        ]
      where
        code = fsep [text "Code:", pPrint _gCode]
        stack = fsep [text "Stack:", pPrint _gStack]
        heap = fsep [text "Heap:", nest 4 $ pPrint _gHeap]
        globals = fsep [text "Globals:", nest 4 $ pPrint _gGlobals]
        dump = fsep [text "Dump:", pPrint _gDump]
        output = fsep [text "Output:", pPrint _gOutput]
