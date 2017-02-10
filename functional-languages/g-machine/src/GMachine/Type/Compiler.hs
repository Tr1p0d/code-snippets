{-# Language DeriveFunctor #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}

module GMachine.Type.Compiler
    ( AlternativesCompiler
    , Compiler
    , GCompiledSC
    , GMCompiler
    , argOffset
    , extendEnvironment
    , extendEnvironment_
    )
  where

import Control.Monad (void)
import Data.Int (Int)
import Data.Word (Word32)

import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (Writer)
import Control.Monad.State (MonadState, execStateT, modify)
import Control.Monad.Writer (MonadWriter, execWriter)

import GMachine.Type.Common (Name)
import GMachine.Type.Core (CoreAlt, CoreExpr)
import GMachine.Type.InstructionSet (GMCode)


type GCompiledSC = (Name, Int, GMCode)

type SCArgOffset = [(Name, Word32)]

type AlternativesCompiler = [CoreAlt] -> Compiler ()

type GMCompiler = CoreExpr -> Compiler ()

-- | Monad that holds the Core Expression and Argument offsets.
newtype Compiler a =
    Compiler { getCompiler :: StateT SCArgOffset (Writer GMCode) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState SCArgOffset
    , MonadWriter GMCode
    )

argOffset :: Word32 -> Compiler ()
argOffset n = modify (\env -> [(v, n+m) | (v,m) <- env])

extendEnvironment :: [Name] -> Compiler Word32
extendEnvironment names = do
    let n = fromIntegral $ length names
    argOffset n
    modify (zip names [0..] ++)
    pure n

extendEnvironment_ :: [Name] -> Compiler ()
extendEnvironment_ = void . extendEnvironment

execCompiler :: GMCompiler -> GMCode
execCompiler = execWriter . flip execStateT [] . getCompiler
