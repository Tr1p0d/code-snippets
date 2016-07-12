module Main where

import Control.Monad ((>=>), replicateM_)
import Data.Maybe (fromJust)

import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V (fromList, unsafeFreeze)
import qualified Data.Vector.Mutable as MV (IOVector, replicate)

import GeneticPipeline.Crossover
import GeneticPipeline.GeneticPipeline
import GeneticPipeline.Join
import GeneticPipeline.Selection

dummyPopulation :: IO (Population (MV.IOVector Int, Double))
dummyPopulation = do
    mvList <- mapM (\x -> MV.replicate 5 x) [0..9]
    return $ V.fromList $ zip mvList [10..19]

main :: IO ()
main = runGeneticPipeline pipeline'
  where
    pipeline = do
        tournament
        -- =>= debug
        --    mv1 <- lift $ (V.unsafeThaw $ V.fromList [0..9])
        --    mv2 <- lift $ (V.unsafeThaw $ V.fromList [10..19])
        --    yieldGP (mv1 :: MV.IOVector Int)
        --    yieldGP (mv2 :: MV.IOVector Int)
        =>>= pointCrossover
    pipeline' = (pipeline =><= tournament $ priorityJoin 0.8) =>>= consumer

    tournament = lift dummyPopulation >>= tournamentSelection 3
    consumer = replicateM_ 10 $ awaitGP >>= lift . printMVector . fromJust

    printMVector = V.unsafeFreeze >=> print
    --debug = do
    --    Just x <- awaitGP
    --    lift $ putStr "Selected: "
    --    lift $ printMVector x
    --    yieldGP x
    --    debug
