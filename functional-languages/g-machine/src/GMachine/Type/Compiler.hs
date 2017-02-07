{-# Language DeriveFunctor #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}

module GMachine.Type.Compiler
    ( Compiler
    , GCompiledSC
    , GMCompiler
    , argOffset
    )
  where

import Data.Int (Int)
import Data.Word (Word32)

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (Writer)
import Control.Monad.State (MonadState, modify)
import Control.Monad.Writer (MonadWriter)

import GMachine.Type.Common (Name)
import GMachine.Type.Core (CoreExpr)
import GMachine.Type.InstructionSet (GMCode)


type GCompiledSC = (Name, Int, GMCode)

type SCArgOffset = [(Name, Word32)]

type GMCompiler = Compiler ()

-- | Monad that holds the Core Expression and Argument offsets.
newtype Compiler a =
    Compiler { _getCompiler :: ReaderT CoreExpr (StateT SCArgOffset (Writer GMCode)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CoreExpr
    , MonadState SCArgOffset
    , MonadWriter GMCode
    )

argOffset :: Word32 -> Compiler ()
argOffset n = modify (\env -> [(v, n+m) | (v,m) <- env])
