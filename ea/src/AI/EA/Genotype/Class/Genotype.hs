{-# LANGUAGE TypeFamilies #-}

module AI.EA.Genotype.Class.Genotype
  where

class Genotype a where
    type CrossParams a :: *
    type MutateParams a :: *
    type InitParams a :: *

    cross ::  CrossParams a -> a -> a -> IO (a, a)
    mutate ::  MutateParams a -> a -> IO a
    init :: InitParams a -> IO a
