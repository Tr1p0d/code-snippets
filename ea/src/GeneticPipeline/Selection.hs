module GeneticPipeline.Selection where

import Control.Monad (forever)

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector as V ((!), length)
import qualified Data.Vector.Mutable as MV (IOVector, clone)

import GeneticPipeline.GeneticPipeline

type TournamentSize = Int

tournamentSelection
    :: TournamentSize
    -> Population (a, Double)
    -> Selection a
tournamentSelection tSize population = forever $ do
    lift (getRandomIndividual >>= tournament (tSize - 1))
     >>= yieldGP . fst
  where
    getRandomIndividual = do
        let upperI = V.length population - 1
        (population V.!) <$> getRandomR (0, upperI)

    tournament 0 ind = return ind
    tournament i ind@(_, fitness) = do
        ind'@(_, fitness') <- getRandomIndividual
        if fitness > fitness'
        then tournament (i-1) ind'
        else tournament (i-1) ind

tournamentSelection'
    :: TournamentSize
    -> Population (MV.IOVector a, Double)
    -> Selection (MV.IOVector a)
tournamentSelection' tSize population = tournamentSelection tSize population
     =>= deepcopy
  where
    deepcopy = forever $ unsafeAwaitGP >>= lift . MV.clone >>= yieldGP
