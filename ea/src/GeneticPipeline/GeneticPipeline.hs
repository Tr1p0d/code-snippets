{-# LANGUAGE RankNTypes #-}
module GeneticPipeline.GeneticPipeline where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield(Yield), Await(Await))
import qualified Control.Monad.Parallel as MP (MonadParallel(bindM2))
import Data.Functor.Sum (Sum(InL, InR))
import qualified Data.Vector.Mutable as MV (IOVector)
import Data.Void (Void)


type GeneticPipeline a b m r =
    Coroutine (Sum (Await (Maybe a)) (Yield b)) m r

type GeneticSplitter b m r =
    Coroutine (Sum (Await (Maybe b)) (Sum (Yield b) (Yield b))) m r

type GeneticJoin a m r =
    Coroutine (Sum (Sum (Await (Maybe a)) (Await (Maybe a))) (Yield a)) m r

type Selection b = GeneticPipeline Void b IO ()

type GeneticOp a b = GeneticPipeline a b IO ()

type Population a = MV.IOVector a

yieldGP :: Monad m => b -> GeneticPipeline a b m ()
yieldGP = suspend . InR . flip Yield (return ())

awaitGP :: Monad m => GeneticPipeline a b m (Maybe a)
awaitGP = suspend $ InL $ Await return

route
    :: Monad m
    => (forall a' b' c' . (a' -> b' -> m c') -> m a' -> m b' -> m c')
    -> GeneticPipeline a b m r
    -> GeneticPipeline b c m r'
    -> GeneticPipeline a c m r'
route bind t1 t2 = Coroutine (bind proceed (resume t1) (resume t2))
  where
    proceed (Left (InL s)) c =
        return (Left $ InL $ fmap (\x -> route bind x $ Coroutine (return c)) s)
    proceed c (Left (InR s)) =
        return (Left $ InR $ fmap (route bind (Coroutine (return c))) s)
    proceed (Left (InR (Yield b c1))) (Left (InL (Await f))) =
        resume (route bind c1 $ f (Just b))
    proceed (Left (InR (Yield _ _))) (Right y) =
        --resume (c1 route return y)
        return $ Right y
    proceed (Right x) (Left (InL (Await f))) =
        resume (route bind (return x) (f Nothing))
    proceed (Right _) (Right y) = return $ Right y

(=>=)
    :: MP.MonadParallel m
    => GeneticPipeline a b m ()
    -> GeneticPipeline b c m ()
    -> GeneticPipeline a c m ()
(=>=) = route bindM2
  where
    bindM2 f ma mb= do
        va <- ma
        vb <- mb
        f va vb

(=>>=)
    :: MP.MonadParallel m
    => GeneticPipeline a b m ()
    -> GeneticPipeline b c m ()
    -> GeneticPipeline a c m ()
(=>>=) = route MP.bindM2

runGeneticPipeline :: Monad m => GeneticPipeline Void Void m r -> m r
runGeneticPipeline = runCoroutine . mapSuspension (let a=a in a)
