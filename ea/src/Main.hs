module Main where

import Control.Monad ((>=>), replicateM_)
import Data.Maybe (fromJust)

import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V (Vector, fromList, replicate)
--import qualified Data.Vector.Mutable as MV (IOVector, replicate)

import GeneticPipeline.Crossover
import GeneticPipeline.GeneticPipeline
import GeneticPipeline.Join
import GeneticPipeline.Selection

dummyPopulation :: Population (V.Vector Int, Double)
dummyPopulation =
    let vList = map (V.replicate 5) [0..9 :: Int]
    in V.fromList $ zip vList [10..19]

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
    pipeline' = (pipeline >>=<< tournament $ priorityJoin 0.8) =>>= consumer

    tournament = tournamentSelection 3 dummyPopulation
    consumer = replicateM_ 10 $ awaitGP >>= lift . print . fromJust

    --printMVector = V.unsafeFreeze >=> print
    --debug = do
    --    Just x <- awaitGP
    --    lift $ putStr "Selected: "
    --    lift $ printMVector x
    --    yieldGP x
    --    debug
