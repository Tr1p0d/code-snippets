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


