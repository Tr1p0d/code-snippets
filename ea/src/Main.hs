module Main where

import Control.Monad ((>=>), forever, replicateM_)

import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V (fromList, unsafeFreeze, unsafeThaw)

import GeneticPipeline.GeneticPipeline
import GeneticPipeline.Selection
import GeneticPipeline.Crossover

dummyPopulation :: IO (Population (Int, Double))
dummyPopulation = V.unsafeThaw $ V.fromList $ zip [0..9] [10..19]

main :: IO ()
main = runGeneticPipeline pipeline
  where
    pipeline = do
            p <- lift dummyPopulation
            (tournamentSelection 3 p)
        =>>= do
            forever $ awaitGP >>= lift . putStrLn . show
        =>>= do
            pointCrossover
        =>>= do
            replicateM_ 10 $ awaitGP >>= lift . printIndividual

    printIndividual = V.unsafeFreeze >=> print

