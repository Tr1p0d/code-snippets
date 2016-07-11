module GeneticPipeline.Crossover where

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Mutable as MV (IOVector, length, read)

import GeneticPipeline.GeneticPipeline

pointCrossover :: GeneticPipeline (MV.IOVector a) (Maybe (MV.IOVector a)) IO ()
pointCrossover = do
    v11 <- awaitGP
    v12 <- awaitGP
    yieldGP v11
