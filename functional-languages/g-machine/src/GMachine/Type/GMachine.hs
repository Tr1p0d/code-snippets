{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : $Header$
-- Description : The graph machine monad
-- Copyright   : (c) Marek Kidon, 2017
-- License     : GPL-3
-- Maintainer  : marek.kidon@gmail.com
-- Stability   : experimental
-- Portability:  GHC specific language extensions.
--
-- This module contains the graph machine monad and its
-- possible error states.
module GMachine.Type.GMachine
    ( GMachineState
    , GMachineTransition
    , GMachineError
        ( ConditionNotABoolean
        , EmptyDump
        , NotEnoughArguments
        , OutOfInstructions
        )
    , badTransition
    , runMachine
    )
  where

import Control.Monad.Trans.Except (ExceptT(ExceptT), throwE)
import Control.Monad.Trans.Writer (Writer)
import Control.Monad.Writer (MonadWriter)

import GMachine.Type.GMState (GMRun, GMState)
import GMachine.Type.InstructionSet (Instruction)


-- | As a FSM is defined. Each state has input and
-- output transisitions.
type GMachineState = GMState -> Machine GMState

-- | As a FMS transition funtion is defined. New state is a
-- projection of cartesian product of input and current state.
type GMachineTransition = GMState -> Instruction -> Machine GMState

newtype Machine a =
    Machine { runMachine :: ExceptT GMachineError (Writer GMRun) a }
  deriving (Functor, Applicative, Monad, MonadWriter GMRun)

data GMachineError
    = OutOfInstructions
    | ConditionNotABoolean
    | NotEnoughArguments
    | EmptyDump
  deriving (Show)

badTransition :: GMachineError -> Machine a
badTransition = Machine . throwE
