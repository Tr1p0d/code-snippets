module Heap where

import qualified Data.Map as M


newtype Heap a = Heap { _unHeap :: (Int, [Int], M.Map Int a) }
  deriving (Show)

hAlloc :: Heap a -> a -> (Int, Heap a)
hAlloc (Heap (c, f, m)) v =
    let alloc = f !! c in (alloc,Heap ((c+1), f, M.insert alloc v m))

hLookup :: Heap a -> Int -> a
hLookup (Heap (_, _, m)) a = m M.! a

hInitial :: Heap a
hInitial = Heap (0, [0..], M.empty)