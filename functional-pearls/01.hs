import Data.Array
import Data.List
import Data.Word

type Nat = Int

-- {{{ ARRAY BASED SOLUTION ---------------------------------------------------
minFree :: [Nat] -> Nat
minFree = search . checkList

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checkList :: [Nat] -> Array Int Bool
checkList xs = accumArray (||) False (0, n) $ zip (filter (<n) xs) (repeat True)
  where
    n = length xs
-- }}} ARRAY BASED SOLUTION ---------------------------------------------------
-- {{{ DIVIDE AND CONQUER SOLUTION --------------------------------------------
minFree' :: [Nat] -> Nat
minFree' = minFrom 0

minFrom a xs
    | null xs = a
    | length us == b - a = minFrom b vs
    | otherwise = minFrom a us
  where
    b = n + 1 `div` 2
    (us,vs) = partition (<b) xs
    n = length xs
-- }}} DIVIDE AND CONQUER SOLUTION --------------------------------------------
