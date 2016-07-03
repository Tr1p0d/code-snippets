import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans(..))

newtype Trampoline m r = Trampoline
    {bounce :: m (Either (Trampoline m r) r)}

instance Applicative m => Applicative (Trampoline m) where

instance Functor m => Functor (Trampoline m) where

instance Monad m => Monad (Trampoline m) where
    return = Trampoline . return . Right

    t >>= f = Trampoline $ bounce t >>= either leftH rightH
      where
        leftH = return . Left . (>>=f)
        rightH = bounce . f

instance MonadTrans Trampoline where
    lift = Trampoline . liftM Right

pause :: Monad m => Trampoline m ()
pause = Trampoline (return $ Left $ return ())

run :: Monad m => Trampoline m r -> m r
run t = bounce t >>= either run return

--Trampoline (m (Right ()))
--Trampoline (m (Right ()))
--
--Trampoline (m (Left (Trampoline (m (Right ())))))
