module Main where

import Control.Monad (replicateM_)
import Data.Maybe (fromJust)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V (Vector, fromList, replicate)

import Criterion.Main

import GeneticPipeline.Crossover
import GeneticPipeline.GeneticPipeline
import GeneticPipeline.Join
import GeneticPipeline.Mutation
import GeneticPipeline.Selection

dummyPopulation :: Population (V.Vector Int, Double)
dummyPopulation =
    let vList = map (V.replicate 5) [0..9 :: Int]
    in V.fromList $ zip vList [10..99]

main :: IO ()
main = defaultMain
    [ bgroup "parallelPipe"
        [ bench "10" $ whnfIO (geneticPipeline 10)
        , bench "100" $ whnfIO (geneticPipeline 100)
        , bench "1000" $ whnfIO (geneticPipeline 1000)
        ]
    , bgroup "sequentialPipe"
        [ bench "10" $ whnfIO (geneticPipeline' 10)
        , bench "100" $ whnfIO (geneticPipeline' 100)
        , bench "1000" $ whnfIO (geneticPipeline' 1000)
        ]
    ]

geneticPipeline :: Int -> IO ()
geneticPipeline times = runGeneticPipeline pipeline'
  where
    pipeline = do
        tournament
        =>>= pointCrossover
        =>>= (pointMutation (getRandomR (-5,5)) 0.2)
    pipeline' = (pipeline >>=<< tournament $ priorityJoin 0.8) =>>= consumer times

geneticPipeline' :: Int -> IO ()
geneticPipeline' times = runGeneticPipeline pipeline'
  where
    pipeline = do
        tournament
        =>= pointCrossover
        =>= (pointMutation (getRandomR (-5,5)) 0.2)
    pipeline' = (pipeline >=< tournament $ priorityJoin 0.8) =>= consumer times

tournament = tournamentSelection 3 dummyPopulation
--consumer = replicateM_ times $ awaitGP >>= lift . print . fromJust
consumer times = replicateM_ times $ awaitGP >> return ()
