{-# LANGUAGE LambdaCase #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Map as M (Map, map, union, singleton, empty, findWithDefault)
import Data.Maybe
import Data.Tree as T
import Data.Foldable

import Control.Monad.Random.Class
import System.Random

type Substitution = Map String Type

data Type
    = Var String
    | Cons String
    | Arrow Type Type
  deriving (Show)

infixr 7 `Arrow`

s f g x = f x (g x)

k x y = x

i x = x


data Comb
    = S
    | K
    | I
  deriving (Show)

data Matched a = Matched
    { combinator :: a
    , arguments :: [Type]
    }
  deriving (Show)

data MatchedNode a = MatchedNode
    { rType :: Type
    , matched :: Maybe (Matched a)
    }
  deriving (Show)

type MatchedTree a = Tree (MatchedNode a)

showTree :: Show a => MatchedTree a -> String
showTree = drawTree . fmap showNode
  where
    showNode = \case
        MatchedNode t Nothing -> "to be generated" ++ ": " ++ show t
        MatchedNode t (Just a) -> (show $ combinator a) ++ ": " ++ show t

identityTree :: MatchedTree Comb
identityTree =
    Node (MatchedNode (Var "a" `Arrow` Var "a") (Just (Matched S
        [Var "a" `Arrow` Var "b" `Arrow` Var "a"
        , Var "a" `Arrow` Var "b"
        ])))
    [ Node (MatchedNode (Var "a" `Arrow` Var "b" `Arrow` Var "a") Nothing) []
    , Node (MatchedNode (Var "a" `Arrow` Var "b") Nothing) []
    ]

-- {{{ THE UNIFIER ------------------------------------------------------------
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
        s2 = unify (substitute s1 t12) (substitute s1 t22)
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
-- }}} THE UNIFIER ------------------------------------------------------------
randomOneOf :: [a] -> IO a
randomOneOf l = do
    let lastE = length l - 1
    randomE <- randomRIO (0,lastE)
    return $ l !! randomE
