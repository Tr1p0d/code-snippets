{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ((++), length, replicate, init)
import Control.Monad ((>=>))

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, put)
import Data.Vector as V (Vector, replicateM, unsafeFreeze, head, tail)
import qualified Pipes.Prelude as PP
import Pipes
import Pipes.Core
import Pipes.Concurrent
import Pipes.Internal as PI

import AI.EA.Genotype.Class.Genotype
import AI.EA.Genotype.IntVector


type PopulationSize = Int
type Population a = V.Vector a

type MonadEvolution s a = StateT s IO a

-- | sort of it works
main :: IO ()
main = do
    (GAIntMVector evolved) <- execStateT (evolve eParams) undefined
    unsafeFreeze evolved >>= print
  where
    eParams = EvolutionParams
        { populationSize = 10
        , genotypeCrossParams = SinglePointC
        , genotypeInitParams = GAInitIntVector 5 (-10, 10)
        , genotypeMutateParams = Replace (-10, 10)
        , fitness = undefined
        , terminationCondition = undefined
        }

data EvolutionParams a = EvolutionParams
    { populationSize :: PopulationSize
    , genotypeInitParams :: InitParams a
    , genotypeCrossParams :: CrossParams a
    , genotypeMutateParams :: MutateParams a
    , fitness :: a -> IO Double
    , terminationCondition :: a -> IO ()
    }

evolve :: (Genotype a) => EvolutionParams a -> MonadEvolution a ()
evolve params = randomPopulation params >>= evolveWithPopulation params

evolveWithPopulation
    :: (Genotype a)
    => EvolutionParams a
    -> Population a
    -> MonadEvolution a ()
evolveWithPopulation params = lift . evolveWithPopulation' params >=> put
  where
    evolveWithPopulation' EvolutionParams{..} ePopulation = do
        let e1 = V.head ePopulation
            e2 = V.head $ V.tail ePopulation
        fst <$> cross genotypeCrossParams e1 e2

randomPopulation
    :: (Genotype a)
    => EvolutionParams a
    -> MonadEvolution a (Population a)
randomPopulation EvolutionParams{..} =
    lift $ V.replicateM populationSize $ init genotypeInitParams


-- REPRODUCTION PIPELINE


