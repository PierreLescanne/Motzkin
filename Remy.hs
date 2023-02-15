-- © Pierre Lescanne (ENS de Lyon)
module Remy where

import Data.Vector (Vector,(!),(//),take,replicate,fromList,toList)
import Data.List 
import System.Random
import Control.Monad.State

--  == Constants ==
-- The size of the vector used to generate binary trees (for my poor computer).
-- This may be increased for other configurations.
-- This limits to Motzkin trees of size 499.  But on other configurations one may go much further. 
sizeOfVector :: Int
sizeOfVector = 50002

-- The initial vector
initialVector :: Vector Int
initialVector = Data.Vector.replicate sizeOfVector (-1)

-- (external nodes are at even locations, internal nodes are at odd locations numbers)
-- (see Knuth Algorithm R, in TAOCP vol4A, fasicle 4, § 7.2.1.6, page 19)

type Gen = State StdGen

rbt :: Int -> Int -> Gen (Vector Int)
rbt seed 0 = do put (mkStdGen seed)
                return(initialVector // [(0,0)])
rbt seed n =
  do v <- rbt seed (n-1)
     generator <- get
     let (rand, newGenerator) = randomR (0::Double,1) generator
     put newGenerator
     let x = floor (rand * fromIntegral (4*n-3))
          -- x is a random value between 0 and 4n-3 --
         k = x `div` 2
     case even x of
       True -> return(v // [(k,2*n-1),(2*n-1,v!k),(2*n,2*n)])
       False -> return(v // [(k,2*n-1),(2*n-1,2*n),(2*n,v!k)])

-- == TEST ==
aBinTree :: Int -> Int -> Vector Int
aBinTree seed size = Data.Vector.take (2*size+1) $ evalState (rbt seed size) (mkStdGen 0)

aBinTree' :: Int -> Int -> [(Int,Int)] -- the list with its indices
aBinTree' seed size =
  let l = toList $ aBinTree seed size
  in zipWith (,) [0.. (2*size)] l
  

