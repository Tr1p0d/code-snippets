{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor.Identity (Identity (..))

newtype Coroutine s m r =
    Coroutine {resume :: m (Either (s (Coroutine s m r)) r)}

instance (Functor s, Functor m) => Functor (Coroutine s m) where
    fmap = undefined

instance (Functor s, Applicative m) => Applicative (Coroutine s m) where
    pure = undefined
    a <*> b = undefined

instance (Functor s, Monad m) => Monad (Coroutine s m) where
    return = Coroutine . return . Right

    c >>= f = Coroutine $ resume c >>= either handleL handleR
      where
        handleR = resume . f
        handleL = return . Left . fmap (>>=f)

instance MonadTrans (Coroutine s) where
    lift = Coroutine . liftM Right

suspend :: (Functor s, Monad m) => s (Coroutine s m x) -> Coroutine s m x
suspend = Coroutine . return . Left

type Trampoline m r = Coroutine Identity m r
type Generator a m r = Coroutine ((,) a) m r
type Iteratee a m r = Coroutine ((->) a) m r

-- Trampoline
pause :: Monad m => Trampoline m ()
pause = suspend $ Identity $ return ()

runTrampoline :: Monad m => Trampoline m r -> m r
runTrampoline t = resume t >>= either (runTrampoline . runIdentity) return

-- Generator
yield :: Monad m => a -> Generator a m ()
yield a = suspend (a, return ())

runGenerator :: Monad m => Generator a m r -> m ([a], r)
runGenerator = runGenerator' []
  where
    runGenerator' l g = resume g >>= either
        (\(x, cont) -> runGenerator' (x:l) cont)
        (\x -> return (l,x))

-- Iteratee
await :: Monad m => Iteratee a m a
await = suspend $ \a -> return a

runIteratee :: Monad m => [a] -> Iteratee a m r -> m r
runIteratee [] i = resume i >>= either (const $ error "cannot resume") return
runIteratee (a:as) i = resume i >>= either
    (\cont -> runIteratee as $ cont a)
    return

-- EitherT functor
data EitherT f g x
    = LeftT (f x)
    | RightT (g x)
    deriving (Show)

type ReqResp a b = EitherT ((->) a) ((,) b)

instance (Functor f, Functor g) => Functor (EitherT f g) where
    fmap f (LeftT l) = LeftT (fmap f l)
    fmap f (RightT r) = RightT (fmap f r)

-- Transducers
type Transducer a b m r = Coroutine (EitherT ((->) (Maybe a)) ((,) b)) m r

data Void

awaitT :: Monad m => Transducer a b m (Maybe a)
awaitT = suspend $ LeftT return

yieldT :: Monad m => b -> Transducer a b m ()
yieldT b = suspend $ RightT (b, return ())

lift121 :: Monad m => (a -> b) -> Transducer a b m ()
lift121 f = awaitT >>= maybe (return ()) (yieldT . f) >> lift121 f

-- Pipelining Transducers
(=>=) :: Monad m
    => Transducer a b m x
    -> Transducer b c m y
    -> Transducer a c m y
t1 =>= t2 = Coroutine (bindM2 proceed (resume t1) (resume t2))
  where
    proceed (Left (LeftT s)) c =
        return (Left $ LeftT $ fmap (=>= Coroutine (return c)) s)
    proceed c (Left (RightT s)) =
        return (Left $ RightT $ fmap (Coroutine (return c) =>= ) s)
    proceed (Left (RightT (b, c1))) (Left (LeftT f)) =
        resume (c1 =>= f (Just b))
    proceed (Left (RightT (b, c1))) (Right y) =
        resume (c1 =>= return y)
    proceed (Right x) (Left (LeftT f)) =
        resume (return x =>= f Nothing)
    proceed (Right x) (Right y) = return $ Right y

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f m1 m2 = do
    r1 <- m1
    r2 <- m2
    f r1 r2

mapSuspension
    :: (Monad m, Functor s)
    => (forall a. s a -> s' a)
    -> Coroutine s m a
    -> Coroutine s' m a
mapSuspension f cort = Coroutine {resume = liftM map' (resume cort)}
  where
    map' (Right r) = Right r
    map' (Left s) = Left (f $ fmap (mapSuspension f) s)

transducer1 :: Transducer Void Int IO ()
transducer1 = trans' [1..10]
  where
    trans' [] = return ()
    trans' (a:ax) = do
        lift $ putStrLn $ "yielding: " ++ show a
        yieldT a
        trans' ax

transducer2 :: Transducer Int Void IO ()
transducer2 = do
    dsVal <- awaitT
    case dsVal of
        Just x -> do
            lift $ putStrLn $ "got: " ++ show x
            transducer2
        Nothing -> return ()

toTrampoline :: Monad m => Transducer Void Void m r -> Trampoline m r
toTrampoline = mapSuspension undefined

-- Branching
type Splitter a m r
    = Coroutine (EitherT ((->) (Maybe a)) (EitherT ((,) a) ((,) a))) m r

type Join a m r
    = Coroutine (EitherT (EitherT ((->) (Maybe a)) ((->) (Maybe a))) ((,) a)) m r

--newtype Coroutine s m r =
--    Coroutine {resume :: m (Either (s (Coroutine s m r)) r)}
yieldL :: (Monad m) => a -> Splitter a m ()
yieldL a = suspend $ (RightT $ LeftT (a, return ()))

yieldR :: (Monad m) => a -> Splitter a m ()
yieldR a = suspend $ (RightT $ RightT (a, return ()))

awaitL :: Monad m => Join a m (Maybe a)
awaitL = suspend (LeftT $ LeftT return)

awaitR :: Monad m => Join a m (Maybe a)
awaitR = suspend (LeftT $ RightT return)

splitter :: Splitter Int IO ()
splitter = trans' [0..10]
  where
    trans' [] = return ()
    trans' (a:aa:as) = yieldL a >> yieldR aa >> trans' as

joiner :: Join Int IO ()
joiner = do
    lv <- awaitL
    rv <- awaitR
    case lv of
        Just x -> do
            lift $ putStrLn $ "got: " ++ show (rv, lv)
            joiner
        Nothing -> return ()

joinTransduced
    :: (Monad m)
    => Transducer a b m x
    -> Transducer a b m y
    -> Join b m z
    -> Transducer a b m (x,y,z)
joinTransduced t1 t2 j =
    Coroutine (bindM3 proceed (resume t1) (resume t2) (resume j))
  where
    -- | Left is waiting
    proceed (Left (LeftT s)) c j = return (Left $ LeftT $ fmap
            (\x -> joinTransduced x (Coroutine $ return c) (Coroutine $ return j)) s)

    -- | Right is waiting
    proceed c (Left (LeftT s)) j = return (Left $ LeftT $ fmap
            (\x -> joinTransduced (Coroutine $ return c) x (Coroutine $ return j)) s)

    -- | Join is yielding
    proceed c1 c2 (Left (RightT s)) = return (Left $ RightT $ fmap
            (\x -> joinTransduced (Coroutine $ return c1) (Coroutine $ return c2) x) s)

    proceed (Left (RightT (x, c1))) c2 (Left (LeftT (LeftT f))) =
        resume $ joinTransduced c1 (Coroutine $ return c2) (f $ Just x)

    proceed c1 (Left (RightT (x, c2))) (Left (LeftT (RightT f))) =
        resume $ joinTransduced (Coroutine $ return c1) c2 (f $ Just x)

    proceed (Right x) c2 (Left (LeftT (LeftT f))) =
        resume $ joinTransduced (return x) (Coroutine $ return c2) (f Nothing)

    proceed c1 (Right y) (Left (LeftT (RightT f))) =
        resume $ joinTransduced (Coroutine $ return c1) (return y) (f Nothing)

    proceed (Right x) (Right y) (Right z) = return $ Right (x,y,z)

joinTransducedTest = runTrampoline $ toTrampoline $ joinTransduced
    leftT
    rightT
    joinTransducers
    =>= return ()
  where
    leftT = yieldT "Vlevo" >> leftT =>= lift121 id
    rightT = yieldT "Vpravo"
    joinTransducers = do
        y <- awaitR
        lift $ putStrLn (show y)
        joinTransducers

bindM3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bindM3 f m1 m2 m3 = do
    v1 <- m1
    v2 <- m2
    v3 <- m3
    f v1 v2 v3
