import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans (..))

newtype Generator a m x = Generator
    {bounceGen :: m (Either (a, Generator a m x) x)}


instance Applicative m => Applicative (Generator a m) where
    pure = undefined
    a <*> b = undefined

instance Functor m => Functor (Generator a m) where
    fmap = undefined

instance Monad m => Monad (Generator a m) where
    return = Generator . return . Right

    g >>= f = Generator $ bounceGen g >>= either leftH rightH
      where
        leftH (a, cont) = return $ Left (a, cont >>= f)
        rightH = bounceGen . f

instance MonadTrans (Generator a) where
    lift = Generator . liftM Right

yield :: Monad m => a -> Generator a m ()
yield a = Generator $ return $ Left (a, return ())

runGenerator :: Monad m => Generator a m x -> m ([a], x)
runGenerator = runGenerator []
  where
    runGenerator l g = bounceGen g >>= either leftH rightH
      where
        leftH (y, cont) = runGenerator (y:l) cont
        rightH x = return (l, x)


hello = do
    lift $ putStr "Hello, "
    yield 1
    lift $ putStrLn "World!"
    yield 2
