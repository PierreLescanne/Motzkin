module Motzkin where

import Data.Vector (Vector,(!),(//),toList,take,replicate,fromList)
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
-- The size of the vector used to generate binary trees
sizeLimit :: Int
sizeLimit = 5000


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
-- as an infinite list
motzkinL :: [Integer]
motzkinL = 1:1: (zipWith div
                (zipWith (+)
                  (zipWith (*) [5,7..] (tail motzkinL))
                  (zipWith (*) [3,6..] (motzkinL)))
                [4..])

-- With memory
motL :: [Integer]
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


-- Optimized for large value, I use asymptotic approximation of M_n
-- nbTerms is the number of terms in  the asymptotic development
rMtO :: Vector Float -> Int -> Int -> Vector Int
rMtO _ _ 0 = initialVector // [(0,1),(1,0),(2,2)]
rMtO _ _ 1 = initialVector // [(0,1),(1,3),(2,0),(3,2),(4,4)]
rMtO rands nbTerms n =  -- (rands!n) a Float between 0 and 1
 let t1 = 1.0/3.0
     t2 = 1.0/fromIntegral(2*(n-1))
     t3 = 1.0/(fromIntegral (8*(n-1)*(n-1)))
     t4 = (-1.0) /(fromIntegral (48*(n-1)*(n-1)*(n-1)))
     u1 = 1.0
     u2 = (-15.0)/(16.0 * fromIntegral(n))
     u3 = 505.0/(512.0 * fromIntegral(n^2))
     u4 = (-8085.0) / (8192.0 * fromIntegral(n^3))
     v1 = 1.0
     v2 = -15.0/(16.0 * fromIntegral(n-1))
     v3 = 505.0/(512.0 * fromIntegral((n-1)^2))
     v4 = -8085.0 / (8192.0 * fromIntegral((n-1)^3))
     pref = (fromIntegral (2*n+1))/ (fromIntegral (n+2))
     x | nbTerms <= 1 = pref * t1 * v1 / u1
       | nbTerms == 2 = pref * (t1 + t2) * (v1 + v2) /(u1 + u2)
       | nbTerms == 3 = pref * (t1 + t2 +t3) * (v1 + v2 + v3) / (u1 + u2 + u3)
       | nbTerms > 3 = pref * (t1 + t2 + t3 + t4) * (v1 + v2 + v3 + v4) / (u1 + u2 + u3 + u4)
 in case x <= rands ! (3*n) of
             True -> case1O rands nbTerms n
             False -> case2O rands nbTerms n
             
case1O ::Vector Float -> Int -> Int -> Vector Int
case1O _ _ 0 = initialVector
case1O _ _ 1 = initialVector -- 0, 1 are never invoqued and are irrelevant
case1O rands nbTerms n =
  let k = floor ((rands!(3*n+1)) * (fromIntegral (2*n)))
      v | n > upperLimit = rMtO rands 1 (n-1)
        | lowerLimit < n && upperLimit <= n =  rMtO rands 2 (n-1)
        | n <= lowerLimit = rMt rands (n-1)
  in case odd k || odd (v!k) || odd (v!(k-1)) of
        True -> v // [(k,2*n+1),(2*n+1,v!k),(2*n+2,2*n+2)]
        False -> v // [(k-1,2*n+1),(2*n+1,v!(k-1)),(2*n+2,2*n+2)] 

case2O :: Vector Float -> Int -> Int -> Vector Int
case2O _ _ 0 = initialVector
case2O _ _ 1 = initialVector  -- 0, 1 are never invoqued and are irrelevant
case2O rands nbTerms n =
  let r = floor ((rands!(3*n+2)) * (fromIntegral (3*n-6)))
      k = r `div` 3
      c = r `rem` 3
      v | n > upperLimit = rMtO rands 1 (n-2)
        | lowerLimit < n && upperLimit <= n =  rMtO rands 2 (n-2)
        | n <= lowerLimit = rMt rands (n-2)
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
                 let rands = fromList (drop d $ Data.List.take (2*size+3) (randoms g :: [Float]))
                   in print $ Data.Vector.take (2*size+3) (rMt rands size)
-- optimized
testO d nbTerms size = do g <- getStdGen
                          let rands = fromList (drop d $ Data.List.take (2*size+3) (randoms g :: [Float]))
                            in print $ Data.Vector.take (2*size+3) (rMtO rands nbTerms size)

testO' d nbTerms size = do g <- getStdGen
                           let rands = fromList (Data.List.take (2*size+3) (randoms g :: [Float]))
                             in print $ (rMtO rands nbTerms size) ! (2*size+2)
                            
-- approximation
withM :: [Double]
withM = [fromIntegral(motzkin (n-1)) / fromIntegral(motzkin n)
        | n<-[1..]]

withAO :: Int -> [Double]
withAO nbt =
  [let t1 = 1.0/3.0
       t2 = 1.0/fromIntegral(2*(n-1))
       t3 = 1.0/(fromIntegral (8*(n-1)*(n-1)))
       t4 = (-1.0) /(fromIntegral (48*(n-1)*(n-1)*(n-1)))
       u1 = 1.0
       u2 = (-15.0)/(16.0 * fromIntegral(n))
       u3 = 505.0/(512.0 * fromIntegral(n*n))
       u4 = (-8085.0) / (8192.0 * fromIntegral(n*n*n))
       v1 = 1.0
       v2 = -15.0/(16.0 * fromIntegral(n-1))
       v3 = 505.0/(512.0 * fromIntegral((n-1)^2))
       v4 = -8085.0 / (8192.0 * fromIntegral((n-1)^3))
       x | nbt <= 1 = t1 * v1 / u1
         | nbt == 2 = (t1 + t2) * (v1 + v2) /(u1 + u2)
         | nbt == 3 = (t1 + t2 +t3) * (v1 + v2 + v3) / (u1 + u2 + u3)
         | nbt > 3 = (t1 + t2 + t3 + t4) * (v1 + v2 + v3 + v4) / (u1 + u2 + u3 + u4)
    in x   
  | n <- [1..]]


