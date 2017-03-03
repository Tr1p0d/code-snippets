{-# LANGUAGE LambdaCase #-}

import Data.Map as M

data Type
    = Arrow Type Type
    | Var String
    | Cons String
  deriving (Show)

type Substitution = Map String Type

infixr 7 `Arrow`

compose :: Substitution -> Substitution -> Substitution
s1 `compose` s2 = M.map (substitute s2) s1 `M.union` s2

substitute :: Substitution -> Type -> Type
substitute s = \case
    t@(Var x) -> findWithDefault t x s
    t1 `Arrow` t2 -> substitute s t1 `Arrow` substitute s t2
    Cons x -> Cons x

unify :: Type -> Type -> Substitution
unify (t11 `Arrow` t12) (t21 `Arrow` t22) =
    let s1 = unify t11 t21
        s2 = unify t12 t22
    in s2 `compose` s1
unify (Var a) t = M.singleton a t
unify (Cons x) (Cons y)
    | x == y = M.empty
    | otherwise = error "Cannot unify constructors"
unify _ _ = error "Cannot unify"

unifyTails :: Type -> Type -> Type
unifyTails t1 t2 =
    let t12 = getTypeTail t2 t1
        s1 = unify t12 t2
    in substitute s1 t1

getTypeTail :: Type -> Type -> Type
getTypeTail t1 t2@(t21 `Arrow` t22) =
    if typeSize t2 == typeSize t1
    then t2
    else getTypeTail t1 t22

typeSize :: Type -> Int
typeSize (t1 `Arrow` t2) = 1 + typeSize t2
typeSize _ = 1
