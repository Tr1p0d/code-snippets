{-# LANGUAGE NoImplicitPrelude #-}
module GMachine.Type.Core where
-- |
-- Module      : $Header$
-- Description : The core language definition
-- Copyright   : (c) Marek Kidon, 2017
-- License     : GPL-3
-- Maintainer  : marek.kidon@gmail.com
-- Stability   : experimental
-- Portability:  GHC specific language extensions.
--
-- This module contains the core language definition
-- and various type aliases
import Data.Bool (Bool)
import Data.Function (($))
import Data.Word (Word32)
import Text.Show (Show)

import GMachine.Type.Common (Name)

import Prelude (Integer)

type IsRec = Bool

data Expr a
    = EVar Name -- Variables
    | ENum Integer -- Numbers
    -- | This thing was not in the original book, but it is sort of
    -- a valuable simplification.
    | EConstr Integer Integer [Expr a] -- Constructor tag arity expressions
    | EAp (Expr a) (Expr a) -- Applications
    | ELet -- Let (rec) expressions
        IsRec -- boolean with True = recursive,
        (LocalDefinitions a)
        (Expr a) -- Body of let(rec)
    | ECase -- Case expression
        (Expr a) -- Expression to scrutinise
        (Alternatives a)
    | ELam [a] (Expr a) -- Lambda abstractions
  deriving (Show)
type CoreExpr = Expr Name

-- {{{ LOCAL DEFINITIONS ------------------------------------------------------
data LocalDefinition a = LocalDefinition
    { _ldName :: a
    , _ldBody :: Expr a
    }
  deriving (Show)

type LocalDefinitions a = [LocalDefinition a]
type CoreLocalDefinitions = LocalDefinitions Name
-- }}} LOCAL DEFINITIONS ------------------------------------------------------
-- {{{ ALTERNATIVES -----------------------------------------------------------
data Alternative a = Alternative
    { _tag :: Word32
    , _altArguments :: [a]
    , _altBody :: Expr a
    }
  deriving (Show)

type Alternatives a = [Alternative a]
type CoreAlternatives = Alternatives Name
-- }}} ALTERNATIVES -----------------------------------------------------------
-- {{{ PROGRAM ----------------------------------------------------------------
data Supercombinator a = Supercombinator
    { _scName :: Name
    , _scArguments :: [a]
    , _scBody :: Expr a
    }
  deriving (Show)
type CoreSupercombinator = Supercombinator Name

type Program a = [Supercombinator a]
type CoreProgram = Program Name
-- }}} PROGRAM ----------------------------------------------------------------
-- {{{ PRELUDE ----------------------------------------------------------------
prelude :: CoreProgram
prelude =
    [ s
    , k
    , k1
    , i
    , compose
    , twice
    ]

s :: CoreSupercombinator
s = Supercombinator "S" ["f","g","x"] $ EAp (EAp (EVar "f") (EVar "x"))
        (EAp (EVar "g") (EVar "x"))

k :: CoreSupercombinator
k = Supercombinator "K" ["x", "y"] $ EVar "x"

k1 :: CoreSupercombinator
k1 = Supercombinator "K1" ["x", "y" ] $ EVar "y"

i :: CoreSupercombinator
i = Supercombinator "I" ["x"] $ EVar "x"

compose :: CoreSupercombinator
compose = Supercombinator
    "compose" ["f","g","x"] $ EAp (EVar "f") (EAp (EVar "g") (EVar "x"))

twice :: CoreSupercombinator
twice = Supercombinator
    "twice" ["f"] $ EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")
-- }}} PRELUDE ----------------------------------------------------------------
