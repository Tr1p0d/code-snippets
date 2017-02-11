{-# Language DeriveFunctor #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
module GMachine.Type.Compiler
    ( AlternativesCompiler
    , Compiler
    , GCompiledSC
    , GMCompiler
    , LetCompiler
    , argOffset
    , execCompiler
    , extendEnvironment
    , extendEnvironment1
    , extendEnvironment_
    , extendEnvironment1_
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

type LetCompiler = LocalDefs -> GMCompiler

type GMCompiler = CoreExpr -> Compiler ()

type LocalDefs = [(Name, CoreExpr)]

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

extendEnvironment1 :: [Name] -> Compiler Word32
extendEnvironment1 names = do
    let n = fromIntegral $ length names
    argOffset n
    modify (zip names [n-1, n-2 .. 0] ++)
    pure n

extendEnvironment1_ :: [Name] -> Compiler ()
extendEnvironment1_ = void . extendEnvironment1_

execCompiler :: SCArgOffset -> Compiler () -> GMCode
execCompiler env = execWriter . flip execStateT env . getCompiler
