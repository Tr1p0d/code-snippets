{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module EA
  where

import Data.Word (Word32)

import Data.Vector as V (replicateM)
import Data.Void (Void)

import GeneticPipeline.GeneticPipeline

data EvolutionaryParams a = EvolutionaryParams
    { populationSize :: Word32
    , randomIndividual :: IO a
    , fitness :: a -> IO Double
    , reproduction :: Population (a, Double) -> GeneticPipeline Void a IO ()
    , terminationCondition :: TerminationCondition a
    }

data TerminationCondition a = TerminationCondition
    { maxGeneration :: Word32
    , fitEnoughIndividual :: Double -> Bool
    }

ea :: EvolutionaryParams a -> IO ()
ea params@EvolutionaryParams{..} = mkInitPop >>= eaWithPop params
  where
    mkInitPop = V.replicateM (fromIntegral populationSize) randomIndividual

eaWithPop :: EvolutionaryParams a -> Population a -> IO ()
eaWithPop params@EvolutionaryParams{..} pop = do
    evaluated <- mapM (\a -> (a,) <$> fitness a) pop
    _ <- terminateP evaluated
    nextGeneration <- runGeneticPipeline (pipeline' evaluated)
    eaWithPop params nextGeneration
  where
    pipeline' a = reproduction a =>= vectorConsumer populationSize
    terminateP _a = undefined

vectorConsumer :: Word32 -> GeneticPipeline a Void IO (Population a)
vectorConsumer times = V.replicateM (fromIntegral times) unsafeAwaitGP
