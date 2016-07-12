{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (replicateM)

import Control.Lens ((^.))
import Data.Bifunctor (Bifunctor, bimap)
import Data.Monoid (Endo(appEndo,Endo))
import Data.Random (sample, MonadRandom)
import Data.Random.Distribution.Uniform (stdUniform, uniform)
import Data.Random.Distribution.Bernoulli (bernoulli)

import Zippers

data Operation
    = Plus
    | Minus
    deriving (Show)

data Terminal
    = Const Int
    deriving (Show)

main :: IO ()
main = do
    let d = 2
    pair <- (,) <$> full d <*> full d
    --nPair <- subtreeCrossoverUniform pair d
    print pair

sampleTree1 :: GProgram Operation Terminal
sampleTree1 =
    Node 2 Plus
        (Node 1 Minus
            (Leaf 0 (Const 1))
            (Leaf 0 (Const 2))
        )
        (Node 1 Minus
            (Leaf 0 (Const 3))
            (Leaf 0 (Const 4))
        )

sampleTree2 :: GProgram Operation Terminal
sampleTree2 =
    Node 2 Minus
        (Node 1 Plus
            (Leaf 0 (Const 5))
            (Leaf 0 (Const 6))
        )
        (Node 1 Plus
            (Leaf 0 (Const 7))
            (Leaf 0 (Const 8))
        )

full :: (MonadRandom m) => Int -> m (GProgram Operation Terminal)
full 0 = Leaf 0 <$> Const <$> sample stdUniform
full n = Node n <$> arbitrary [Plus, Minus] 2 <*> subProg <*> subProg
  where
    subProg = full $ n - 1

randomDirections :: (MonadRandom m) => Int -> m (NavigationStep op t)
randomDirections l = flattenEM <$> replicateM l (arbitrary [left, right] 2)

subtreeCrossoverPreferLeafs
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int      -- | Uppwer bound
    -> Float    -- | percentil of Leaf preference
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverPreferLeafs ps ub preference = do
    leaf <- sample $ bernoulli preference
    if leaf then subtreeCrossoverLeaf ps else subtreeCrossoverUniformNodes ps ub

subtreeCrossoverUniform
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Uppwer bound
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniform ps ub = subtreeCrossoverUniformGen ps 0 ub

subtreeCrossoverUniformGen
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Lower bound
    -> Int -- | Uppwer bound
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniformGen ps@(p1,p2) lb ub =
    subtreeCrossoverGen ps $ sample $ uniform lb ub

subtreeCrossoverGen
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> m Int
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverGen ps g = do
    r <- g
    let (r1, r2) = commonRegions r ps
    mkProgramTuple <$> arbitrary' r1  <*> arbitrary' r2

subtreeCrossoverLeaf
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverLeaf ps = subtreeCrossoverGen ps $ return 0

subtreeCrossoverUniformNodes
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> Int -- | Uppwer bound
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverUniformNodes ps ub = subtreeCrossoverUniformGen ps 1 ub

commonRegions
    :: Int
    -> (GProgram op t, GProgram op t)
    -> ([GPZipper op t], [GPZipper op t])
commonRegions height = bimap' (subZippers height) . bimap' toGPZipper

--- <<< VARIOUS UTILITY FUNCTIONS ---------------------------------------------

arbitrary :: (MonadRandom m) => [a] -> Int -> m a
arbitrary list elems = (list !!) <$> sample (uniform 0 $ elems - 1)

arbitrary' :: (MonadRandom m) => [a] -> m a
arbitrary' list = arbitrary list (length list)

--- >>> VARIOUS UTILITY FUNCTIONS ---------------------------------------------
