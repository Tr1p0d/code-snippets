module GMachine.Type.Core where

import GMachine.Type.Common (Name)


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
        [(a, Expr a)] -- Definitions
        (Expr a) -- Body of let(rec)
    | ECase -- Case expression
        (Expr a) -- Expression to scrutinise
        [Alternative a] -- Alternatives
    | ELam [a] (Expr a) -- Lambda abstractions
  deriving (Show)
type CoreExpr = Expr Name

type Alternative a = (Integer, [a], Expr a)
type CoreAlternative = Alternative Name
type CoreAlternatives = [CoreAlternative]

type Supercombinator a = (Name, [a], Expr a)
type CoreSupercombinator = Supercombinator Name

type Program a = [Supercombinator a]

type CoreProgram = Program Name

preludes :: CoreProgram
preludes =
    [ ("I", ["x"], EVar "x")
    , ("K", ["x", "y"] , EVar "x")
    , ( "K1", ["x", "y" ], EVar "y" )
    , s
    , ("compose", ["f","g","x"] , EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]

s :: CoreSupercombinator
s = ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
        (EAp (EVar "g") (EVar "x")))
