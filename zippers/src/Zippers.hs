{-# LANGUAGE TemplateHaskell #-}

module Zippers where

import Data.Maybe (isJust)
import Data.Monoid (Endo)

import Control.Lens ((^.), makeLenses)


data GProgram op t
    = Node
        { _height :: Int
        , _nodeData :: op
        , _lSubTree :: GProgram op t
        , _rSubTree :: GProgram op t
        }
    | Leaf
        { _height :: Int
        , _val :: t
        }
    deriving (Show)

makeLenses ''GProgram

data GPContext op t
    = RSubProgram Int op (GProgram op t)
    | LSubProgram Int op (GProgram op t)
    deriving (Show)

makeLenses ''GPContext

data GPZipper op t = Focus (GProgram op t) [GPContext op t]
    deriving (Show)

makeLenses ''GPZipper


type NavigationStep op t = GPZipper op t -> GPZipper op t

-- <<< UTILITY FUNCS ----------------------------------------------------------

subZippers :: Int -> GPZipper op t -> [GPZipper op t]
subZippers h z@(Focus a _)
    | (a ^. height) < h = []
    | (a ^. height) == h = [z]
    | otherwise = subZippers h (left z) ++ subZippers h (right z)

-- <<< CONSTRUCTORS -----------------------------------------------------------

toGPZipper :: GProgram op t -> GPZipper op t
toGPZipper p = Focus p []

viewFocus :: GPZipper op t -> GProgram op t
viewFocus (Focus p a) = p

fromGPZipper :: GPZipper op t -> GProgram op t
fromGPZipper (Focus p []) = p
fromGPZipper a = fromGPZipper $ up a

-- <<< MOVES ------------------------------------------------------------------

maybeRight :: GPZipper op t -> Maybe (GPZipper op t)
maybeRight (Focus a ctxs) = case a of
    Node op i l r -> Just $ Focus r $ RSubProgram op i l:ctxs
    Leaf _ _ -> Nothing

maybeLeft :: GPZipper op t -> Maybe (GPZipper op t)
maybeLeft (Focus a ctxs) = case a of
    Node op i l r -> Just $ Focus l $ LSubProgram op i r:ctxs
    Leaf _ _ -> Nothing

maybeUp :: GPZipper op t -> Maybe (GPZipper op t)
maybeUp (Focus a []) = Nothing
maybeUp (Focus a (ctx:ctxs)) = Just $ case ctx of
    RSubProgram h op l -> Focus (Node (updateH h $ a ^. height) op l a) ctxs
    LSubProgram h op r -> Focus (Node (updateH h $ a ^. height) op a r) ctxs

right :: GPZipper op t -> GPZipper op t
right z@(Focus a ctxs) = case a of
    Node op i l r -> Focus r $ RSubProgram op i l:ctxs
    Leaf _ _ -> z

left :: GPZipper op t -> GPZipper op t
left z@(Focus a ctxs) = case a of
    Node op i l r -> Focus l $ LSubProgram op i r:ctxs
    Leaf _ _ -> z

up :: GPZipper op t -> GPZipper op t
up z@(Focus _ []) = z
up (Focus a (ctx:ctxs)) = case ctx of
    RSubProgram h op l -> Focus (Node (updateH h $ a ^. height) op l a) ctxs
    LSubProgram h op r -> Focus (Node (updateH h $ a ^. height) op a r) ctxs

updateH parent = max parent . (+1)

-- >>> MOVES ------------------------------------------------------------------

switch
    :: (GPZipper op t, GPZipper op t)
    -> (GPZipper op t, GPZipper op t)
switch (Focus p1 c1, Focus p2 c2) = (Focus p2 c1, Focus p1 c2)

changeOp :: op -> GPZipper op t -> Maybe (GPZipper op t)
changeOp op (Focus a c) = case a of
    Node i op' l r -> Just $ Focus (Node i op l r) c
    Leaf _ _ -> Nothing
