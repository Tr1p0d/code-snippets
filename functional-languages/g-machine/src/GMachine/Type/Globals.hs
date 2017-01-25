{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module GMachine.Type.Globals
    ( Globals
    , empty
    , getGlobals
    )
  where

import qualified Data.Map as M (Map, empty, toList)

import Control.Lens (makeLenses)
import Text.PrettyPrint
    ( text
    , vcat
    , quotes
    , (<+>)
    , (<>)
    )
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

import GMachine.Type.Common (Name)
import GMachine.Type.Address (Address)


newtype Globals = Globals {_getGlobals :: M.Map Name Address}
makeLenses ''Globals

empty :: Globals
empty = Globals M.empty

instance Pretty Globals where
    pPrint = vcat . map pPrintGlobal . M.toList . _getGlobals
      where
        pPrintGlobal (symbol, address) =
            quotes (text symbol) <> text ":" <+> pPrint address
