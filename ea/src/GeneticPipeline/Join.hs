module GeneticPipeline.Join where

import Control.Monad (forever)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)

import GeneticPipeline.GeneticPipeline

priorityJoin :: Double -> GeneticJoin a IO ()
priorityJoin priority = forever $ do
    select <- lift $ getRandomR (0, 1)>>= return . (<priority)
    if select
    then do
        lift $ putStr "Yielding Left"
        awaitJoinL >>= yieldJoin . (\(Just x) -> x)
    else do
        lift $ putStr "Yielding Right"
        awaitJoinR >>= yieldJoin . (\(Just x) -> x)
