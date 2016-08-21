module Main where

import Control.Monad.Trans.State.Lazy (execStateT)
import Control.Monad.Random (getRandomR)
import qualified Data.Vector as V (unsafeFreeze)
import Data.Vector.Mutable as MV (replicateM)

import EA
import GeneticPipeline.Crossover
import GeneticPipeline.GeneticPipeline
import GeneticPipeline.Mutation
import GeneticPipeline.Selection


main :: IO ()
main = execStateT (ea params) emptyProgress >>= print . _generation
  where
    params = EvolutionaryParams
        { populationSize = 20
        , randomIndividual = MV.replicateM 10 randomGene
        , fitness = \i -> (fromIntegral . sum) <$> V.unsafeFreeze i
        , terminationCondition = TerminationCondition 100 (==55.0)
        , reproduction = \g -> tournamentSelection 5 g
            =>= pointCrossover
            =>= pointMutation randomGene 0.05
        }
      where
        randomGene = getRandomR (-5,5) :: IO Int
    emptyProgress = EvolutionProgress
        { _generation = 0
        , _bestIndividual = undefined
        }
