{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ((++), length, replicate)
import Data.Vector as M (Vector, (++),  modify, length, replicate, splitAt)
import Data.Vector.Mutable as MV (IOVector, length, write, modify, replicate)
import Control.Monad.Random

class Genotype a where
    type CrossParams a :: *
    type MutateParams a :: *

    cross :: MonadRandom m => CrossParams a -> a -> a -> m (a, a)
    mutate :: MonadRandom m => MutateParams a -> a -> m a


-- Integer Vector instance
newtype GAIntVector = GAIntVector {vector :: (M.Vector Int)} deriving (Show)

data CrossType
    = SinglePointC
    | TwoPointC
    | MultiPointC

data GACrossIntVector = GACrossIntVector
    { crossType :: CrossType }

data GAIntMutationType
    = Replace
    | ModifyIncDec

data GAMutateIntVector = SinglePointM
    { mutationType :: GAIntMutationType }

instance Genotype GAIntVector where
    type CrossParams GAIntVector = GACrossIntVector
    type MutateParams GAIntVector = GAMutateIntVector

    mutate (SinglePointM t) (GAIntVector v) = do
        (index :: Int) <- getRandomR (0, M.length v - 1)
        case t of
            ModifyIncDec -> do
                (val :: Int) <- getRandomR (0,1)
                return $ GAIntVector $ M.modify
                    (\v' -> MV.modify v' (+val) index) v
            _ -> undefined

    cross (GACrossIntVector t) (GAIntVector v1) (GAIntVector v2) = do
        case t of
            SinglePointC -> do
                (i1 :: Int) <- getRandomR (0, M.length v1 - 1)
                let (v11, v12) = M.splitAt i1 v1
                    (v21, v22) = M.splitAt i1 v2
                return (GAIntVector (v11 ++ v22), GAIntVector (v21 ++ v12))

main :: IO ()
main = do
   -- mutate (SinglePointM ModifyIncDec) (GAIntVector $ M.replicate 5 (1 :: Int))
   cross (GACrossIntVector SinglePointC)
    (GAIntVector $ M.replicate 5 (1 :: Int))
    (GAIntVector $ M.replicate 5 (2 :: Int)) >>= print

