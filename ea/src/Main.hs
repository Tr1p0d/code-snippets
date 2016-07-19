module Main where

import Control.Monad (replicateM_, forever)
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

--main :: IO ()
--main = defaultMain
--    [ bgroup "=>>="
--        [ bench "100" $ whnfIO (benchRoute (=>>=) 10)
--        ]
--    , bgroup "=>="
--        [ bench "100" $ whnfIO (benchRoute (=>=) 10)
--        ]
--    ]

benchRoute f times = runGeneticPipeline $ producer `f` consumer times
  where
    producer = forever $ do
        (lift $ getRandomR (-100, 100::Int)) >>= yieldGP

--main = benchRoute (=>=) 1000000
main = geneticPipeline 1000000

geneticPipeline :: Int -> IO ()
geneticPipeline times = runGeneticPipeline pipeline'
  where
    pipeline = do
        tournament
        =>= pointCrossover
        =>= (pointMutation (getRandomR (-5,5)) 0.2)
    --pipeline' = (pipeline >=< tournament $ priorityJoin 0.8) =>= consumer times
    pipeline' = pipeline =>= consumer times

    tournament = tournamentSelection 3 dummyPopulation

consumer times = replicateM_ times $ awaitGP >> return ()
