import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield(Yield), Await(Await))
import Control.Monad.Random (getRandomR)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Sum (Sum(InL, InR))
import qualified Data.Vector as V (fromList, unsafeThaw, unsafeFreeze)
import qualified Data.Vector.Mutable as MV (IOVector, length, read)
import Data.Void (Void)

type GeneticPipeline a b m r = Coroutine (Sum (Await (Maybe a)) (Yield b)) m r

type Selection b = GeneticPipeline Void b IO ()

type GeneticOp a b = GeneticPipeline a b IO ()

type TournamentSize = Int
type Population a = MV.IOVector a

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

yieldGP :: Monad m => b -> GeneticPipeline a b m ()
yieldGP = suspend . InR . flip Yield (return ())

awaitGP :: Monad m => GeneticPipeline a b m (Maybe a)
awaitGP = suspend $ InL $ Await return

(=>=)
    :: Monad m
    => GeneticPipeline a b m ()
    -> GeneticPipeline b c m ()
    -> GeneticPipeline a c m ()
c1 =>= c2 = Coroutine (bindM2 proceed (resume c1) (resume c2))
  where
    proceed (Left (InL s)) c =
        return (Left $ InL $ fmap (=>= Coroutine (return c)) s)
    proceed c (Left (InR s)) =
        return (Left $ InR $ fmap (Coroutine (return c) =>= ) s)
    proceed (Left (InR (Yield b c1))) (Left (InL (Await f))) =
        resume (c1 =>= f (Just b))
    proceed (Left (InR (Yield b c1))) (Right y) =
        resume (c1 =>= return y)
    proceed (Right x) (Left (InL (Await f))) =
        resume (return x =>= f Nothing)
    proceed (Right x) (Right y) = return $ Right y

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb= do
    va <- ma
    vb <- mb
    f va vb

dummyPopulation :: IO (Population (Int, Double))
dummyPopulation = V.unsafeThaw $ V.fromList $ zip [0..9] [10..19]

runGeneticPipeline :: Monad m => GeneticPipeline Void Void m r -> m r
runGeneticPipeline = runCoroutine . mapSuspension undefined

main = return ()
