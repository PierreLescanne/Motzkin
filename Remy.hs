module Remy where

import Data.Vector (Vector,(!),(//),take,replicate,fromList)
import Data.List 
import System.Random

--  == Constants ==
-- The size of the vector used to generate binary trees (for my poor computer).
-- This may be increased for other configurations.
-- This limits to Motzkin trees of size 499.  But on other configurations one may go much further. 
sizeOfVector :: Int
sizeOfVector = 1002

-- The initial vector
initialVector :: Vector Int
initialVector = Data.Vector.replicate sizeOfVector (-1)

-- (external nodes are even numbers, internal nodes are odd numbers)
-- (see Knuth Algorithm R, in TAOCP vol4A, fasicle 4, § 7.2.1.6, page 19)

rbt :: Vector Float -> Int -> Vector Int
rbt _ 0 = initialVector // [(0,0)]
rbt rands n =
  let x = floor ((rands!n) * fromIntegral (4*n-3))
   -- x is a random value between 0 and 4n-3 --
      v = rbt rands (n-1)
      k = x `div` 2
  in case even x of
    True -> v // [(k,2*n-1),(2*n-1,v!k),(2*n,2*n)]
    False -> v // [(k,2*n-1),(2*n-1,2*n),(2*n,v!k)]

-- == TEST ==
-- by changing d, one changes the random sequence
test d size = do g <- getStdGen
                 let rands = fromList $ drop d $ Data.List.take (2*size+1) (randoms g :: [Float])
                   in print $ Data.Vector.take (2*size+1) (rbt rands size)
