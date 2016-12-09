{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module EA
  where

import Data.Word (Word32)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, get, modify)
import qualified Data.Map as M (fromList)
import Data.Vector as V (replicateM, toList)
import Data.Void (Void)

import Control.Lens ((%~), (^.), makeLenses)

import GeneticPipeline.GeneticPipeline

data EvolutionaryParams a = EvolutionaryParams
    { populationSize :: Word32
    , randomIndividual :: IO a
    , fitness :: a -> IO Double
    , reproduction :: EvaluatedPopulation a -> GeneticPipeline Void a IO ()
    , terminationCondition :: TerminationCondition a
    }

data TerminationCondition a = TerminationCondition
    { maxGeneration :: Word32
    , fitEnoughIndividual :: Double -> Bool
    }

data EvolutionProgress a = EvolutionProgress
    { _generation :: Word32
    , _bestIndividual :: a
    }
makeLenses ''EvolutionProgress

type EvolutionRun a = StateT (EvolutionProgress a) IO

ea :: EvolutionaryParams a -> EvolutionRun a ()
ea params@EvolutionaryParams{..} = lift mkInitPop >>= eaWithPop params
  where
    mkInitPop = V.replicateM (fromIntegral populationSize) randomIndividual

eaWithPop :: EvolutionaryParams a -> Population a -> EvolutionRun a ()
eaWithPop params@EvolutionaryParams{..} pop = do
    evaluated' <- lift $ mapM (\a -> (,a) <$> fitness a) (toList pop)
    let evaluated = M.fromList evaluated'
    terminate <- terminateP
    if terminate
    then return ()
    else do
        printProgress
        nPop <- lift $ runGeneticPipeline (pipeline' evaluated)
        incGeneration >> eaWithPop params nPop
  where
    incGeneration = modify $ generation %~ (+1)
    pipeline' a = reproduction a =>= vectorConsumer populationSize
    terminateP = do
        s <- get
        return $ (s ^. generation) == maxGeneration terminationCondition
    printProgress = do
        s <- get
        lift $ print $ _generation s

vectorConsumer :: Word32 -> GeneticPipeline a Void IO (Population a)
vectorConsumer times = V.replicateM (fromIntegral times) unsafeAwaitGP
