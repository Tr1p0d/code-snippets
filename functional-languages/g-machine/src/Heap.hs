module Heap where

import qualified Data.Map as M


newtype Heap a = Heap { _unHeap :: (Int, [Int], M.Map Int a) }

instance Show a => Show (Heap a) where
    show (Heap (_, _, m)) = show m

hAlloc :: Heap a -> a -> (Int, Heap a)
hAlloc (Heap (c, next:free, m)) v =
    (next, (Heap ((c+1), free, M.insert next v m)))

hLookup :: Heap a -> Int -> a
hLookup (Heap (_, _, m)) a = m M.! a

hInitial :: Heap a
hInitial = Heap (0, [0..], M.empty)
