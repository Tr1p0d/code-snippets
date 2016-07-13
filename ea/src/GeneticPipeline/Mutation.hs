module GeneticPipeline.Mutation where

import Control.Monad (forever)
import Data.Maybe (fromJust)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import Data.Vector as V (Vector, thaw, unsafeFreeze)
import Data.Vector.Mutable as MV (length, unsafeModify)

import GeneticPipeline.GeneticPipeline

pointMutation
    :: IO a
    -> Double
    -> GeneticPipeline (V.Vector a) (V.Vector a) IO ()
pointMutation rElemAction ratio = forever $ do
    v <- awaitGP >>= lift . thaw . fromJust
    lift $ singlePointMutate v
    lift (unsafeFreeze v) >>= yieldGP
  where
    singlePointMutate v = do
        mutationPoint <- getRandomR (0, MV.length v - 1)
        x <- rElemAction
        p <- perform
        if p then (MV.unsafeModify v (const x) mutationPoint) else return ()

    perform = (<ratio) <$> getRandomR (0, 1)
