module Main where

import Control.Monad (replicateM_)

import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V (fromList, unsafeThaw)

import GeneticPipeline.GeneticPipeline
import GeneticPipeline.Selection

dummyPopulation :: IO (Population (Int, Double))
dummyPopulation = V.unsafeThaw $ V.fromList $ zip [0..9] [10..19]

main :: IO ()
main = runGeneticPipeline pipeline
  where
    pipeline = do
        p <- lift dummyPopulation
        (tournamentSelection 3 p)
        =>>= do
            replicateM_ 10 $ awaitGP >>= lift . putStrLn . show
            return ()
