module Motzkin where

import PreMotzkin
import Data.Vector (Vector,(!),(//),take,replicate,fromList)
import Data.List 
import System.Random

-- ==============================
-- Generating Motzkin trees
-- ==============================
{-- This associated with a paper
--
Linear random generation of Motzkin trees
Pierre Lescanne
École Normale Supérieure de Lyon,
LIP (UMR 5668 CNRS ENS Lyon UCBL)
--}

--  == Constants ==
-- The limit with which we work
sizeLimit :: Int
sizeLimit = 26000

-- The initial vector
-- the length of the vector is 2n+3 where n is the size of the Motzkin tree.
initialVector :: Vector Int
initialVector = Data.Vector.replicate (2*sizeLimit+3) (-1)

-- The limits from which one changes from raw calculation to approximations
-- See § "Efficient choice between case1 and case2" in the paper
upperLimit = 1200
lowerLimit = 600

-- == Motzkin numbers ==
-- Motzkin numbers from sequence A001006 on OEIS

-- With memory
motL :: [Integer]                      -- The memory
motL = [motzkin n | n <- [0..]]

motzkin :: Int -> Integer
motzkin 0 = 1
motzkin 1 = 1
motzkin n = ((2*fIn+1)*(motL!!(n-1)) + 3*(fIn-1)*(motL!!(n-2))) `div` (fIn+2)
  where fIn = fromIntegral n

-- with terminal calls
mtzk :: Int -> (Integer,Integer)
mtzk 0 = (1,1)
mtzk n = let (a,b) = mtzk (n-1)
         in (b, ((2*fIn+3)*b+3*fIn*a) `div` (fIn+3)) where fIn = fromIntegral n

mtzkin :: Int -> Integer
mtzkin n = fst $ mtzk n

-- == Motzkin trees as vectors ==
{-- We follow the construction of
Serge Dulucq, Jean-Guy Penaud:
Interprétation bijective d'une récurrence des nombres de Motzkin.
Discret. Math. 256(3): 671-676 (2002)
The reference is my draft.
--}

rMt :: Vector Float -> Int -> Vector Int
rMt _ 0 = initialVector // [(0,1),(1,0),(2,2)]
rMt _ 1 = initialVector // [(0,1),(1,3),(2,0),(3,2),(4,4)]
rMt rands n =  -- (rands!n) a Float between 0 and 1
 let floatToInteger x n = floor (x * (fromIntegral n))
     r = floatToInteger (rands!(3*n)) ((fromIntegral (n+2))*(motzkin n))
 in case r <= (fromIntegral (2*n+1)) * (motzkin (n-1)) of
             True -> case1 rands n
             False -> case2 rands n

-- (even k) means that the marked item is a right child
-- (even (v!k)) means that its label is even, hence it is a leaf
-- (even (v!(k-1)) means that the label of its left sister is even,
--                 hence it is a leaf
-- Therefore {(even k) && (even (v!k) && (even (v!(k-1))} is configuration 7 
case1 ::Vector Float -> Int -> Vector Int
case1 _ 0 = initialVector
case1 _ 1 = initialVector -- 0, 1 are never invoqued and are irrelevant
case1 rands n =
  let k = floor ((rands!(3*n+1)) * (fromIntegral (2*n)))
      v = rMt rands (n-1)
  in case odd k || odd (v!k) || odd (v!(k-1)) of
        True -> v // [(k,2*n+1),(2*n+1,v!k),(2*n+2,2*n+2)]
        False -> v // [(k-1,2*n+1),(2*n+1,v!(k-1)),(2*n+2,2*n+2)] 

case2 :: Vector Float -> Int -> Vector Int
case2 _ 0 = initialVector
case2 _ 1 = initialVector  -- 0, 1 are never invoqued and are irrelevant
case2 rands n =
  let r = floor ((rands!(3*n+2)) * (fromIntegral (3*n-6)))
      k = r `div` 3
      c = r `rem` 3
      v = rMt rands (n-2)
  in case c < 2 of
    True -> v // [(2*k+1,2*n-1),(2*k+2,2*n+1),(2*n-1,2*n),(2*n,2*n+2),(2*n+1,v!(2*k+1)),(2*n+2,v!(2*k+2))]
    False -> v // [(2*k+1,2*n-1),(2*k+2,2*n+1),(2*n-1,v!(2*k+1)),(2*n,v!(2*k+2)),(2*n+1,2*n),(2*n+2,2*n+2)]

-- Calling the vector of precomputed values ratioM
rMtFast :: Vector Double -> Int -> Vector Int
rMtFast _ 0 = initialVector // [(0,1),(1,0),(2,2)]
rMtFast _ 1 = initialVector // [(0,1),(1,3),(2,0),(3,2),(4,4)]
rMtFast rands n =  -- (rands!n) a Float between 0 and 1
  case ratioM!n <= rands!n of
        True -> case1Fast rands n
        False -> case2Fast rands n

-- (even k) means that the marked item is a right child
-- (even (v!k)) means that its label is even, hence it is a leaf
-- (even (v!(k-1)) means that the label of its left sister is even,
--                 hence it is a leaf
-- Therefore {(even k) && (even (v!k) && (even (v!(k-1))} is configuration 7 
case1Fast ::Vector Double -> Int -> Vector Int
case1Fast _ 0 = initialVector
case1Fast _ 1 = initialVector -- 0, 1 are never invoqued and are irrelevant
case1Fast rands n =
  let k = floor ((rands!(3*n+1)) * (fromIntegral (2*n)))
      v = rMtFast rands (n-1)
  in case odd k || odd (v!k) || odd (v!(k-1)) of
        True -> v // [(k,2*n+1),(2*n+1,v!k),(2*n+2,2*n+2)]
        False -> v // [(k-1,2*n+1),(2*n+1,v!(k-1)),(2*n+2,2*n+2)] 

case2Fast :: Vector Double -> Int -> Vector Int
case2Fast _ 0 = initialVector
case2Fast _ 1 = initialVector  -- 0, 1 are never invoqued and are irrelevant
case2Fast rands n =
  let r = floor ((rands!(3*n+2)) * (fromIntegral (3*n-6)))
      k = r `div` 3
      c = r `rem` 3
      v = rMtFast rands (n-2)
  in case c < 2 of
    True -> v // [(2*k+1,2*n-1),(2*k+2,2*n+1),(2*n-1,2*n),(2*n,2*n+2),(2*n+1,v!(2*k+1)),(2*n+2,v!(2*k+2))]
    False -> v // [(2*k+1,2*n-1),(2*k+2,2*n+1),(2*n-1,v!(2*k+1)),(2*n,v!(2*k+2)),(2*n+1,2*n),(2*n+2,2*n+2)]

--  The asymptotic algorithm

rMtAsympt :: Vector Float -> Int -> Vector Int
rMtAsympt _ 0 = initialVector // [(0,1),(1,0),(2,2)]
rMtAsympt _ 1 = initialVector // [(0,1),(1,3),(2,0),(3,2),(4,4)]
rMtAsympt rands n | rands ! (3*n) <= 2.0/3.0 = case1Asympt rands n
                  | otherwise = case2Asympt rands n
 
case1Asympt ::Vector Float -> Int -> Vector Int
case1Asympt _ 0 = initialVector
case1Asympt _ 1 = initialVector -- 0, 1 are never invoqued and are irrelevant
case1Asympt rands n =
  let k = floor ((rands!(3*n+1)) * (fromIntegral (2*n)))
      v = rMtAsympt rands (n-1)
  in case odd k || odd (v!k) || odd (v!(k-1)) of
        True -> v // [(k,2*n+1),(2*n+1,v!k),(2*n+2,2*n+2)]
        False -> v // [(k-1,2*n+1),(2*n+1,v!(k-1)),(2*n+2,2*n+2)] 

case2Asympt :: Vector Float -> Int -> Vector Int
case2Asympt _ 0 = initialVector
case2Asympt _ 1 = initialVector  -- 0, 1 are never invoqued and are irrelevant
case2Asympt rands n =
  let r = floor ((rands!(3*n+2)) * (fromIntegral (3*n-6)))
      k = r `div` 3
      c = r `rem` 3
      v = rMtAsympt rands (n-2)
  in case c < 2 of
    True -> v // [(2*k+1,2*n-1),(2*k+2,2*n+1),(2*n-1,2*n),(2*n,2*n+2),(2*n+1,v!(2*k+1)),(2*n+2,v!(2*k+2))]
    False -> v // [(2*k+1,2*n-1),(2*k+2,2*n+1),(2*n-1,v!(2*k+1)),(2*n,v!(2*k+2)),(2*n+1,2*n),(2*n+2,2*n+2)]

-- == TEST ==
-- by changing d, one changes the random sequence
test d size = do g <- getStdGen
                 let rands = fromList (drop d $ Data.List.take (3*size+3+d) (randoms g :: [Float]))
                   in print $ Data.Vector.take (2*size+3) (rMt rands size)

test' d size = do g <- getStdGen
                  let rands = fromList (drop d $ Data.List.take (3*size+3+d) (randoms g :: [Float]))
                    in print $ (rMt rands size) ! (2*size+2)
                   
-- speedy
testS d size = do g <- getStdGen
                  let rands = fromList (drop d $ Data.List.take (3*size+3+d) (randoms g :: [Double]))
                    in print $ Data.Vector.take (2*size+3) (rMtFast rands size)
                    
testS' d size = do g <- getStdGen
                   let rands = fromList (drop d $ Data.List.take (3*size+3+d) (randoms g :: [Double]))
                     in print $ (rMtFast rands size) ! (2*size+2)

