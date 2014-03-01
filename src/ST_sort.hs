-- Krisztian Pinter, 2014
import Control.Monad as CM
import Data.Vector.Storable as VS
import Data.Vector.Storable.Mutable as VSM
import Control.Monad.ST.Safe as ST
import Data.STRef
import Control.Monad.Primitive
import Control.Monad.Loops

sort :: Vector Int -> Vector Int
sort vec = runST $ do
  v <- thaw vec
  insertionSortST v
  freeze v

insertionSortST :: (Ord a, Storable a) =>
  MVector s a -> ST s ()
insertionSortST v = do
  j <- newSTRef 0
  CM.forM_ [1..(VSM.length v)-1] (\i-> do
    modifySTRef j $ const i
    whileM_ (cond j)  $ do
      jval <- readSTRef j
      swap v (jval-1) jval
      modifySTRef j (+(-1))
    )
  where
    cond j = do
      jval <- readSTRef j
      if jval>0 then
        do
          x1 <- VSM.read v $ jval-1
          x2 <- VSM.read v jval
          return $ x1 > x2
        else return False