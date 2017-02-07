{-# Language DeriveFunctor #-}
{-# Language GeneralizedNewtypeDeriving #-}

module GMachine.Type.Compiler
    ( GCompiledSC
    , Compiler
    )
  where

import Data.Int (Int)
import Data.Word (Word32)

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (State)
import Control.Monad.State (MonadState)

import GMachine.Type.Common (Name)
import GMachine.Type.Core (CoreExpr)
import GMachine.Type.InstructionSet (GMCode)


type GCompiledSC = (Name, Int, GMCode)

type SCArgOffset = [(Name, Word32)]

newtype Compiler a =
    Compiler { _getCompiler :: ReaderT CoreExpr (State SCArgOffset) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CoreExpr
    , MonadState SCArgOffset
    )
