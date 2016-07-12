{-# LANGUAGE RankNTypes #-}
module GeneticPipeline.GeneticPipeline where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Yield(Yield), Await(Await))
import qualified Control.Monad.Parallel as MP (MonadParallel(bindM2))
import Data.Functor.Sum (Sum(InL, InR))
import qualified Data.Vector as V (Vector)
import Data.Void (Void)


type GeneticPipeline a b m r =
    Coroutine (Sum (Await (Maybe a)) (Yield b)) m r

type GeneticSplitter b m r =
    Coroutine (Sum (Await (Maybe b)) (Sum (Yield b) (Yield b))) m r

type GeneticJoin a m r =
    Coroutine (Sum (Sum (Await (Maybe a)) (Await (Maybe a))) (Yield a)) m r

type Selection b = GeneticPipeline Void b IO ()

type GeneticOp a b = GeneticPipeline a b IO ()

type Population a = V.Vector a

yieldGP :: Monad m => b -> GeneticPipeline a b m ()
yieldGP = suspend . InR . flip Yield (return ())

awaitGP :: Monad m => GeneticPipeline a b m (Maybe a)
awaitGP = suspend $ InL $ Await return

runGeneticPipeline :: Monad m => GeneticPipeline Void Void m r -> m r
runGeneticPipeline = runCoroutine . mapSuspension (let a=a in a)

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

-- Join
awaitJoinL :: Monad m => GeneticJoin a m (Maybe a)
awaitJoinL = suspend $ InL $ InL $ Await return

awaitJoinR :: Monad m => GeneticJoin a m (Maybe a)
awaitJoinR = suspend $ InL $ InR $ Await return

yieldJoin :: Monad m => a -> GeneticJoin a m ()
yieldJoin = suspend . InR . flip Yield (return ())

join :: Monad m
    => (forall a' b' c' d' . (a' -> b' -> c' -> m d')
        -> m a' -> m b' -> m c' -> m d')
    -> GeneticPipeline a b m r
    -> GeneticPipeline a b m r'
    -> GeneticJoin b m r''
    -> GeneticPipeline a b m r''
join bind t1 t2 j = Coroutine (bind proceed (resume t1) (resume t2) (resume j))
  where
    proceed (Left (InL s)) c2 j' = return (Left $ InL $ fmap
        (\x -> join bind x (Coroutine $ return c2) (Coroutine $ return j')) s)
    proceed c1 (Left (InL s)) j' = return (Left $ InL $ fmap
        (\x -> join bind (Coroutine $ return c1) x (Coroutine $ return j')) s)
    proceed c1 c2 (Left (InR s)) = return (Left $ InR $ fmap
        (\x -> join bind (Coroutine $ return c1) (Coroutine $ return c2) x) s)
    proceed (Left (InR (Yield x c1))) c2 (Left (InL (InL (Await f)))) =
        resume $ join bind c1 (Coroutine $ return c2) (f $ Just x)
    proceed c1 (Left (InR (Yield x c2))) (Left (InL (InR (Await f)))) =
        resume $ join bind (Coroutine $ return c1) c2 (f $ Just x)
    proceed (Right x) c2 (Left (InL (InL (Await f)))) =
        resume $ join bind (return x) (Coroutine $ return c2) (f Nothing)
    proceed c1 (Right x) (Left (InL (InR (Await f)))) =
        resume $ join bind (Coroutine $ return c1) (return x) (f Nothing)
    --proceed (Left (InR _)) (Left (InR _)) (Right z) = return $ Right z
    proceed _ _ (Right z) = return $ Right z

(=><=) :: Monad m
    => GeneticPipeline a b m r
    -> GeneticPipeline a b m r'
    -> GeneticJoin b m r''
    -> GeneticPipeline a b m r''
(=><=) = join bindM3
  where
    bindM3 f m1 m2 m3 = do
        v1 <- m1
        v2 <- m2
        v3 <- m3
        f v1 v2 v3
