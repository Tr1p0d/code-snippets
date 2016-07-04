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
