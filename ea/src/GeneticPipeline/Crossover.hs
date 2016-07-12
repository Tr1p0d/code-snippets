module GeneticPipeline.Crossover where

import Control.Monad (forever)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Mutable as MV
    (IOVector, length, unsafeWrite, unsafeRead)

import GeneticPipeline.GeneticPipeline

pointCrossover :: GeneticPipeline (MV.IOVector a) (Maybe (MV.IOVector a)) IO ()
pointCrossover = forever $ do
    v11 <- awaitGP
    v12 <- awaitGP
    lift $ sequence_ $ singlePointCrossover <$> v11 <*> v12
    yieldGP v11
    yieldGP v12
  where
    singlePointCrossover v1 v2 = do
        crossoverPoint <- getRandomR (0, MV.length v1 - 1)
        mapM_ (unsafeSwapElements v1 v2) [0..crossoverPoint]

    unsafeSwapElements v1 v2 ix = do
        e1 <- MV.unsafeRead v1 ix
        MV.unsafeRead v2 ix >>= MV.unsafeWrite v1 ix
        MV.unsafeWrite v1 ix e1
