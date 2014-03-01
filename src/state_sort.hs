-- Krisztian Pinter, 2014
import Control.Monad.State
import Control.Monad
import Control.Monad.Loops

{-
for i ← 1 to length(A)
    j ← i
    while j > 0 and A[j-1] > A[j]
        swap A[j] and A[j-1]
        j ← j - 1
-}

insertionSort :: [Int] -> [Int]
insertionSort ls = fst $ execState (
  forM_ [1..(length ls)-1] (\i-> do
    modify $ \(ls,j) -> (ls,i)
    while (\(ls,j) -> j>0 && ls!!(j-1) > ls!!j) $
      modify $ \(ls,j) -> (swap (j-1) j ls, j-1)
    )) (ls, 0)

while cond = whileM_ (gets cond)

swap :: Int -> Int -> [a] -> [a]
swap i j ls
  | i /= j = xs ++ [head zs] ++ tail ys ++ [head ys] ++ tail zs
  | otherwise = ls
  where
    (i',j') = if i < j then (i,j) else (j,i)
    (xs,ys') = splitAt i' ls
    (ys,zs) = splitAt (j'-i') ys'