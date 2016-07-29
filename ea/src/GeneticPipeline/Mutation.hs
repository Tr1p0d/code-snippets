module GeneticPipeline.Mutation where

import Control.Monad (forever)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import Data.Vector.Mutable as MV (IOVector, length, unsafeModify)

import GeneticPipeline.GeneticPipeline

pointMutation
    :: IO a
    -> Double
    -> GeneticPipeline (MV.IOVector a) (MV.IOVector a) IO ()
pointMutation rElemAction ratio = forever $ do
    Just v <- awaitGP
    lift $ singlePointMutate v
    yieldGP v
  where
    singlePointMutate v = do
        mutationPoint <- getRandomR (0, MV.length v - 1)
        x <- rElemAction
        p <- perform
        if p then (MV.unsafeModify v (const x) mutationPoint) else return ()

    perform = (<ratio) <$> getRandomR (0, 1)
