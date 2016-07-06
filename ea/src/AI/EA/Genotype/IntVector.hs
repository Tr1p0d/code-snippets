{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module AI.EA.Genotype.IntVector where

import Prelude hiding ((++), length, replicate)

--import Data.Vector as V (Vector)
import Data.Vector.Mutable as MV
    (IOVector, length, unsafeWrite, replicateM, unsafeRead)
import Data.Vector.Generic.Mutable as GMV (exchange)
import Control.Monad.Random (getRandomR)

import AI.EA.Genotype.Class.Genotype


newtype GAIntMVector = GAIntMVector {mvector :: MV.IOVector Int}

data GACrossIntVector
    = SinglePointC
    | TwoPointC
    | MultiPointC

data GAMutateIntVector
    = Replace (Int, Int)
    | ModifyIncDec

data GAInitIntVector = GAInitIntVector Int (Int, Int)

instance Genotype GAIntMVector where
    type CrossParams GAIntMVector = GACrossIntVector
    type MutateParams GAIntMVector = GAMutateIntVector
    type InitParams GAIntMVector = GAInitIntVector

    cross SinglePointC v1'@(GAIntMVector v1) v2'@(GAIntMVector v2) = do
        index <- getRandomR (0, MV.length v1 - 1)
        _ <- vSwap (v1, v2) index $ MV.length v1
        return (v1', v2')

    cross _ _ _ = undefined

    mutate (Replace b) v'@(GAIntMVector v) = do
        index <- getRandomR (0, MV.length v - 1)
        (val :: Int) <- getRandomR b
        MV.unsafeWrite v index val
        return v'

    mutate _ _ = undefined

    init (GAInitIntVector gSize gElemRange) = do
        GAIntMVector <$> MV.replicateM gSize randomE
      where
        randomE = getRandomR gElemRange

vSwap :: (IOVector a, IOVector a) -> Int -> Int -> IO (IOVector a, IOVector a)
vSwap (v1, v2) n m
   | n == m = return (v1, v2)
   | otherwise = do
       e1 <- MV.unsafeRead v1 n
       e2 <- GMV.exchange v2 n e1
       MV.unsafeWrite v1 n e2
       vSwap (v1, v2) (n + 1)  m
