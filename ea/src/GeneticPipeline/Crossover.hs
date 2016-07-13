module GeneticPipeline.Crossover where

import Control.Monad (forever)
import Data.Maybe (fromJust)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import Data.Vector as V (Vector, thaw, unsafeFreeze)
import Data.Vector.Mutable as MV (length, unsafeRead, unsafeWrite)

import GeneticPipeline.GeneticPipeline

pointCrossover :: GeneticPipeline (V.Vector a) (V.Vector a) IO ()
pointCrossover = forever $ do
    v1 <- awaitGP >>= lift . thaw . fromJust
    v2 <- awaitGP >>= lift . thaw . fromJust
    lift $ singlePointCrossover v1 v2
    lift (unsafeFreeze v1) >>= yieldGP
    lift (unsafeFreeze v2) >>= yieldGP
  where
    singlePointCrossover v1 v2 = do
        crossoverPoint <- getRandomR (0, MV.length v1 - 1)
        mapM_ (unsafeSwapElements v1 v2) [0..crossoverPoint]

    unsafeSwapElements v1 v2 ix = do
        e1 <- MV.unsafeRead v1 ix
        MV.unsafeRead v2 ix >>= MV.unsafeWrite v1 ix
        MV.unsafeWrite v2 ix e1
