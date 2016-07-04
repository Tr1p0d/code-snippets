import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans (..))

newtype Iteratee a m x = Iteratee
    {bounceIter :: m (Either (a -> Iteratee a m x) x)}

instance Functor m => Functor (Iteratee a m) where
    fmap = undefined

instance Applicative m => Applicative (Iteratee a m) where
    pure = undefined
    a <*> b = undefined

instance Monad m => Monad (Iteratee a m) where
    return = Iteratee . return . Right

    i >>= f = Iteratee $ bounceIter i >>= either handleL handleR
      where
        handleR = bounceIter . f
        handleL cont = return $ Left ((>>=f) . cont)

instance MonadTrans (Iteratee a) where
    lift = Iteratee . liftM Right

await :: Monad m => Iteratee a m a
await = Iteratee (return $ Left return)

runIteratee :: Monad m => [a] -> Iteratee a m x -> m x
runIteratee [] i = bounceIter i >>= either
    (const $ error "no more values")
    return
runIteratee (a:as) i = bounceIter i >>= either leftH rightH
  where
    leftH cont = runIteratee as (cont a)
    rightH = return

hello = do
    lift $ putStr "Ahoj, "
    a <- await
    lift $ putStr a
    lift $ putStrLn "Svete!"
