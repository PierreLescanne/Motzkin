module Motzkin where

import Data.Vector (Vector,(!),(//),toList,take,replicate)
import System.Random

-- ==============================
-- Generating Motzkin trees
-- ==============================
--  == Constants ==
-- The size of the vector used to generate binary trees (for my poor computer).
-- This may be increased for other configurations.
-- This limits to Motzkin trees of size 499.  But on other configurations one may go much further. 
sizeOfVector :: Int
sizeOfVector = 1002

-- The initial vector
initialVector :: Vector Int
initialVector = Data.Vector.replicate sizeOfVector (-1)

-- == Motzkin numbers ==
-- Motzkin numbers form sequence A001006 on OEIS
motzkin :: [Integer]
motzkin = 1:1: (zipWith div
                (zipWith (+)
                  (zipWith (*) [5,7..] (tail motzkin))
                  (zipWith (*) [3,6..] (motzkin)))
                [4..])
          
-- == Motzkin trees as vectors ==
{-- We follow the construction of
Serge Dulucq, Jean-Guy Penaud:
Interprétation bijective d'une récurrence des nombres de Motzkin.
Discret. Math. 256(3): 671-676 (2002)
The reference is my draft.
--}

rMt :: [Float] -> Int -> Vector Int
rMt _ 0 = initialVector // [(0,1),(1,0),(2,2)]
rMt _ 1 = initialVector // [(0,1),(1,3),(2,0),(3,2),(4,4)]
rMt rands n =  -- (rands!!n) a Float between 0 and 1
 let floatToInteger x n = floor (x * (fromIntegral n))
     r = floatToInteger (rands!!(3*n)) ((fromIntegral (n+2))*(motzkin!!n))
 in case r <= (fromIntegral (2*n+1)) * (motzkin !! (n-1)) of
             True -> case1 rands n
             False -> case2 rands n

-- (even k) means that the marked item is a right child
-- (even (v!k)) means that its label is even, hence it is a leaf
-- (even (v!(k-1)) means that the label of its left sister is even,
--                 hence it is a leaf
-- Therefore {(even k) && (even (v!k) && (even (v!(k-1))} is configuration 7 
case1 ::[Float] -> Int -> Vector Int
case1 _ 0 = initialVector
case1 _ 1 = initialVector -- 0, 1 are never invoqued and are irrelevant
case1 rands n =
  let k = floor ((rands!!(3*n+1)) * (fromIntegral (2*n)))
      v = rMt rands (n-1)
  in case odd k || odd (v!k) || odd (v!(k-1)) of
        True -> v // [(k,2*n+1),(2*n+1,v!k),(2*n+2,2*n+2)]
        False -> v // [(k-1,2*n+1),(2*n+1,v!(k-1)),(2*n+2,2*n+2)] 

case2 :: [Float] -> Int -> Vector Int
case2 _ 0 = initialVector
case2 _ 1 = initialVector  -- 0, 1 are never invoqued and are irrelevant
case2 rands n =
  let r = floor ((rands!!(3*n+2)) * (fromIntegral (3*n-6)))
      k = r `div` 3
      c = r `rem` 3
      v = rMt rands (n-2)
  in case c < 2 of
    True -> v // [(2*k+1,2*n-1),(2*k+2,2*n+1),(2*n-1,2*n),(2*n,2*n+2),(2*n+1,v!(2*k+1)),(2*n+2,v!(2*k+2))]
    False -> v // [(2*k+1,2*n-1),(2*k+2,2*n+1),(2*n-1,v!(2*k+1)),(2*n,v!(2*k+2)),(2*n+1,2*n),(2*n+2,2*n+2)]

-- == TEST ==
-- by changing d, one changes the random sequence
test d size = do g <- getStdGen
                 let rands = drop d (randoms g :: [Float])
                   in print $ Data.Vector.take (2*size+3) (rMt rands size)

