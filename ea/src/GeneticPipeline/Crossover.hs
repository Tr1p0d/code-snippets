module GeneticPipeline.Crossover where

import Control.Monad (forever)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Mutable as MV
    (IOVector, length, unsafeWrite, unsafeRead)

import GeneticPipeline.GeneticPipeline

pointCrossover :: GeneticPipeline (MV.IOVector a) (MV.IOVector a) IO ()
pointCrossover = forever $ do
    Just v1 <- awaitGP
    Just v2 <- awaitGP
    lift $ singlePointCrossover v1 v2
    yieldGP v1
    yieldGP v2
  where
    singlePointCrossover v1 v2 = do
        crossoverPoint <- getRandomR (0, MV.length v1 - 1)
        mapM_ (unsafeSwapElements v1 v2) [0..crossoverPoint]

    unsafeSwapElements v1 v2 ix = do
        e1 <- MV.unsafeRead v1 ix
        MV.unsafeRead v2 ix >>= MV.unsafeWrite v1 ix
        MV.unsafeWrite v2 ix e1
