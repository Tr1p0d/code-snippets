module GeneticPipeline.Mutation where

import Control.Monad (forever)
import Data.Maybe (fromJust)
import System.Random (Random)

import Control.Monad.Random (getRandom, getRandomR)
import Control.Monad.Trans.Class (lift)
import Data.Vector as V (Vector, thaw, unsafeFreeze)
import Data.Vector.Mutable as MV (length, unsafeModify)

import GeneticPipeline.GeneticPipeline

pointMutation
    :: (Random a)
    => GeneticPipeline (V.Vector a) (V.Vector a) IO ()
pointMutation = forever $ do
    v <- awaitGP >>= lift . thaw . fromJust
    lift $ singlePointMutate v
    lift (unsafeFreeze v) >>= yieldGP
  where
    singlePointMutate v = do
        mutationPoint <- getRandomR (0, MV.length v - 1)
        x <- getRandom
        MV.unsafeModify v (const x) mutationPoint
