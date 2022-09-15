module Motzkin where

import PreMotzkin
import Data.Vector (Vector,(!),(//),take,replicate,fromList)
import Data.List 
import System.Random
import Control.Monad.State

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
sizeLimit = 100003

-- The initial vector
-- the length of the useful part of the vector is 2n+3 where n is the size of the Motzkin tree.
initialVector :: Vector Int
initialVector = Data.Vector.replicate (2*sizeLimit+3) (-1)

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

type Gen = State StdGen

rMt :: Int -> Int -> Gen(Vector Int)
rMt seed 0 = do put (mkStdGen seed)
                return (initialVector // [(0,1),(1,0),(2,2)])
rMt seed 1 = do put (mkStdGen seed)
                return (initialVector // [(0,1),(1,3),(2,0),(3,2),(4,4)])
rMt seed n =
  do generator <- get
     let (rand, newGenerator) = randomR (0::Double,1) generator
         floatToInteger x n = floor (x * (fromIntegral n))
         r = floatToInteger rand ((fromIntegral (n+2))*(motzkin n))
         b = r <= (fromIntegral (2*n+1)) * (motzkin (n-1))
     put newGenerator
     case b of
       True -> case1 seed n
       False -> case2 seed n

-- (even k) means that the marked item is a right child
-- (even (v!k)) means that its label is even, hence it is a leaf
-- (even (v!(k-1)) means that the label of its left sister is even,
--                 hence it is a leaf
-- Therefore {(even k) && (even (v!k) && (even (v!(k-1))} is configuration 7 
case1 :: Int -> Int -> Gen (Vector Int)
case1 seed 0 = do return initialVector
case1 seed 1 = do return initialVector -- 0, 1 are never invoqued
case1 seed n =
  do generator <- get
     let (rand, newGenerator) = randomR (0::Double,1) generator
         k = floor (rand * (fromIntegral (2*n)))
     v <- rMt seed (n-1)
     put newGenerator
     case odd k || odd (v!k) || odd (v!(k-1)) of
       True -> return (v // [(k,2*n+1),(2*n+1,v!k),(2*n+2,2*n+2)])
       False -> return (v // [(k-1,2*n+1),(2*n+1,v!(k-1)),(2*n+2,2*n+2)])

case2 :: Int-> Int -> Gen(Vector Int)
case2 seed 0 = do return initialVector
case2 seed 1 = do return initialVector  -- 0, 1 are never invoqued
case2 seed n =
    do generator <- get
       let (rand, newGenerator) = randomR (0::Double,1) generator
           r = floor (rand * (fromIntegral (3*n-6)))
           k = r `div` 3
           c = r `rem` 3
       v <- rMt seed (n-2)
       put newGenerator
       case c < 2 of
         True -> return (v // [(2*k+1,2*n-1),(2*k+2,2*n+1),
                                (2*n-1,2*n),(2*n,2*n+2),
                                (2*n+1,v!(2*k+1)),(2*n+2,v!(2*k+2))])
         False -> return (v // [(2*k+1,2*n-1),(2*k+2,2*n+1),
                                 (2*n-1,v!(2*k+1)),(2*n,v!(2*k+2)),
                                 (2*n+1,2*n),(2*n+2,2*n+2)])

-- Calling the vector of precomputed values ratioM
rMtFast :: Int -> Int -> Gen (Vector Int)
rMtFast seed 0 = do put (mkStdGen seed)
                    return $ initialVector // [(0,1),(1,0),(2,2)]
rMtFast seed 1 = do put (mkStdGen seed)
                    return $ initialVector // [(0,1),(1,3),(2,0),(3,2),(4,4)]
rMtFast seed n =  
   do generator <- get
      let (rand, newGenerator) = randomR (0::Double,1) generator
      put newGenerator
      case ratioM!n <= rand of
        True -> case1Fast seed n
        False -> case2Fast seed n
case1Fast ::Int -> Int -> Gen(Vector Int)
case1Fast seed 0 = do put (mkStdGen seed)
                      return $ initialVector
case1Fast seed 1 = do put (mkStdGen seed)
                      return $ initialVector -- 0, 1 are never invoqued
case1Fast seed n =
   do generator <- get
      let (rand, newGenerator) = randomR (0::Double,1) generator
          k = floor (rand * (fromIntegral (2*n)))
      v <- rMtFast seed (n-1)
      put newGenerator
      case odd k || odd (v!k) || odd (v!(k-1)) of
       True -> return $ v // [(k,2*n+1),(2*n+1,v!k),(2*n+2,2*n+2)]
       False -> return $ v // [(k-1,2*n+1),(2*n+1,v!(k-1)),(2*n+2,2*n+2)] 
case2Fast :: Int -> Int -> Gen(Vector Int)
case2Fast seed 0 = do put (mkStdGen seed)
                      return $ initialVector
case2Fast seed 1 = do put (mkStdGen seed)
                      return $ initialVector  -- 0, 1 are never invoqued
case2Fast seed n =
    do generator <- get
       let (rand, newGenerator) = randomR (0::Double,1) generator
           r = floor (rand * (fromIntegral (3*n-6)))
           k = r `div` 3
           c = r `rem` 3
       v <- rMtFast seed (n-2)
       put newGenerator
       case c < 2 of
         True -> return $ v // [(2*k+1,2*n-1),(2*k+2,2*n+1),
                                (2*n-1,2*n),(2*n,2*n+2),
                                (2*n+1,v!(2*k+1)),(2*n+2,v!(2*k+2))]
         False -> return $ v // [(2*k+1,2*n-1),(2*k+2,2*n+1),
                                 (2*n-1,v!(2*k+1)),(2*n,v!(2*k+2)),
                                 (2*n+1,2*n),(2*n+2,2*n+2)]

-- == TEST ==
aMotzTree seed size = Data.Vector.take (2*size+3) $ evalState (rMtFast seed size) (mkStdGen 0)
