module Main where

import Control.Monad (replicateM_)
import Data.Maybe (fromJust)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V (fromList, unsafeFreeze)
import Data.Vector.Mutable as MV (IOVector, replicate)
import Data.Void (Void)

import GeneticPipeline.Crossover
import GeneticPipeline.GeneticPipeline
import GeneticPipeline.Mutation
import GeneticPipeline.Selection


main :: IO ()
main = geneticPipeline 30

geneticPipeline :: Int -> IO ()
geneticPipeline times = runGeneticPipeline pipeline
  where
    pipeline = do
        tournament
        =>= pointCrossover
        =>= (pointMutation (getRandomR (-5,5)) 0.2)
        =>= consumer times

    tournament = lift dummyPopulation >>= tournamentSelection' 3

consumer :: (Show a) => Int -> GeneticPipeline (MV.IOVector a) Void IO ()
consumer times = replicateM_ times $ awaitGP
    >>= lift . V.unsafeFreeze . fromJust
    >>= lift . print

dummyPopulation :: IO (Population (MV.IOVector Int, Double))
dummyPopulation = do
     vList <- mapM (MV.replicate 5) [0..1 :: Int]
     return $ V.fromList (zip vList [10..])
