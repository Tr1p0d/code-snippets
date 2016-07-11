module GeneticPipeline.Selection where

import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import qualified Data.Vector.Mutable as MV (length, read)

import GeneticPipeline.GeneticPipeline

type TournamentSize = Int

tournamentSelection
    :: TournamentSize
    -> Population (a, Double)
    -> Selection (a, Double)
tournamentSelection tSize population = do
    lift (getRandomIndividual >>= tournament (tSize - 1)) >>= yieldGP
    tournamentSelection tSize population
  where
    getRandomIndividual = do
        let upperI = MV.length population - 1
        getRandomR (0, upperI) >>= MV.read population

    tournament 0 ind = return ind
    tournament i ind@(_, fitness) = do
        ind'@(_, fitness') <- getRandomIndividual
        if fitness > fitness'
        then tournament (i-1) ind'
        else tournament (i-1) ind
