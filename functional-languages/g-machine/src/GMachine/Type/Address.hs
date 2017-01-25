{-# LANGUAGE LambdaCase #-}
module GMachine.Type.Address
    ( Address(Addr)
    , GMachine.Type.Address.null
    , nullAddr
    , withAddress
    )
  where

import Data.Word (Word32)
import Text.Show (Show)

import Text.PrettyPrint (text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))


data Address
    = Null
    | Addr Word32
  deriving (Show)

nullAddr :: Address
nullAddr = Null

null :: Address -> Bool
null = \case
    Null -> True
    _ -> False

withAddress :: b -> (Word32 -> b) -> Address -> b
withAddress def f = \case
    Null -> def
    Addr index -> f index

instance Pretty Address where
    pPrint addr = text $ show addr
