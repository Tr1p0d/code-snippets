{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (replicateM)

import Data.Bifunctor (Bifunctor, bimap)
import Data.Monoid (Endo(appEndo,Endo))
import Data.Random (sample, MonadRandom)
import Data.Random.Distribution.Uniform (stdUniform, uniform)
import Data.Random.Distribution.Exponential (exponential)

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
            (Leaf (Const 1))
            (Leaf (Const 2))
        )
        (Node 1 Minus
            (Leaf (Const 3))
            (Leaf (Const 4))
        )

sampleTree2 :: GProgram Operation Terminal
sampleTree2 =
    Node 2 Minus
        (Node 1 Plus
            (Leaf (Const 1))
            (Leaf (Const 2))
        )
        (Leaf (Const 3)
        )

full :: (MonadRandom m) => Int -> m (GProgram Operation Terminal)
full 0 = Leaf <$> Const <$> sample stdUniform
full n = Node n <$> arbitrary [Plus, Minus] 2 <*> subProg <*> subProg
  where
    subProg = full $ n - 1

randomDirections :: (MonadRandom m) => Int -> m (NavigationStep op t)
randomDirections l = flattenEM <$> replicateM l (arbitrary [left, right] 2)

--subtreeCrossoverExponential
--    :: (MonadRandom m)
--    => (GProgram op t, GProgram op t)
--    -> Int      -- | Depth
--    -> Float    -- | Lambda
--    -> m (GProgram op t, GProgram op t)
--subtreeCrossoverExponential ps = (subtreeCrossoverGen ps .) . generator
--  where
--    generator depth lambda = do
--        er <- round <$> sample (exponential lambda)
--        return $ if er > depth then depth else er
--
--subtreeCrossoverUniform
--    :: (MonadRandom m)
--    => (GProgram op t, GProgram op t)
--    -> Int -- | pair
--    -> m (GProgram op t, GProgram op t)
--subtreeCrossoverUniform ps = subtreeCrossoverGen ps . sample . uniform 1

subtreeCrossoverGen
    :: (MonadRandom m)
    => (GProgram op t, GProgram op t)
    -> m Int
    -> m (GProgram op t, GProgram op t)
subtreeCrossoverGen ps g = do
    r <- g
    let (r1, r2) = commonRegions r ps
    mkProgramTuple <$> arbitrary' r1  <*> arbitrary' r2
  where
    mkProgramTuple = ((bimap' fromGPZipper . switch) . ) . (, )

commonRegions
    :: Int
    -> (GProgram op t, GProgram op t)
    -> ([GPZipper op t], [GPZipper op t])
commonRegions height = bimap' (subZippers height) . bimap' toGPZipper

--- <<< VARIOUS UTILITY FUNCTIONS ---------------------------------------------

flattenEM :: [a -> a] -> a -> a
flattenEM = appEndo . mconcat . map Endo

bimap' :: (Bifunctor p) => (a -> b) -> p a a -> p b b
bimap' a = bimap a a

arbitrary :: (MonadRandom m) => [a] -> Int -> m a
arbitrary list elems = (list !!) <$> sample (uniform 0 $ elems - 1)

arbitrary' :: (MonadRandom m) => [a] -> m a
arbitrary' list = arbitrary list (length list)

--- >>> VARIOUS UTILITY FUNCTIONS ---------------------------------------------
