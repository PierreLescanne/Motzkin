-- © Pierre Lescanne (ENS de Lyon)
module Schroeder where
import Data.Vector (Vector,(!),(//),take,replicate,toList)
import Data.List 
import Data.Maybe
import System.Random
import Control.Monad.State

-- Schröder-Hipparchus numbers
schröderTab :: [Integer]
schröderTab = [schröder n | n<-[0..]]

schröder :: Int -> Integer
schröder 0 = 0
schröder 1 = 1
schröder n =
  let fI = fromIntegral
  in ((fI (6*n-9) * schröderTab!!(n-1)) - (fI (n-3) * schröderTab!!(n-2))) `div` (fI n)

--  == Constants ==
-- The size of the vector used to generate Schröder trees.
sizeOfVector :: Int
sizeOfVector = 100001

initV :: Vector (Int,Bool)
initV = Data.Vector.replicate sizeOfVector (-1,False)

-- Random Schröder trees
type Gen = State StdGen

rst :: Int -> Int -> Gen (Vector (Int,Bool))
rst seed 0 = do put (mkStdGen seed) 
                return (initV // [(0,(0,False))])
rst seed n =
  let g = rst seed (n-1)
      body generator =
        do v <- g
           let (rand, newGenerator) = randomR (0::Double,1) generator
               x = floor (rand * fromIntegral (6*n-4))
               -- x is a random value between 0 and 6n - 4
               k = x `div` 3
           put newGenerator
           case x `mod` 3 of
             0 {-L1-} -> return (v // [(k,(2*n-1,False)),
                                       (2*n-1,(2*n,False)),
                                       (2*n,v!k)]) -- L1 
             1 {-L2-} -> case odd(fst (v!k)) of
               True {- not a leaf-}-> return (v// [(k,(2*n-1,False)),
                                                   (2*n-1,(2*n,False)),
                                                   (2*n,(fst(v!k),True))])
               False -> case odd k  of 
                  True {- the leaf is on the left -} -> case snd (v ! (k+1)) of
                    False {- the other link is black-} ->
                      return (v//[(k,v!(k+1)),
                                  (k+1,(2*n-1,True)),
                                  (2*n-1,v!k),(2*n,(2*n,False))])
        {-Failure-} True {- the other link is white -} -> body newGenerator
                  False {- the leaf is on the right -} ->
                    return (v//[(k,(2*n-1,True)),
                                (2*n-1,v!k),
                                (2*n,(2*n,False))])
             2 {-R1-} ->  return (v // [(k,(2*n-1,False)),
                                        (2*n-1,v!k),
                                        (2*n,(2*n,False))])
  in do generator <- get
        body generator

-- == TEST ==
-- for rst
aSchroederTree :: Int -> Int -> [(Int, (Int, Bool))]
aSchroederTree seed size = 
  let v = evalState (rst seed size) (mkStdGen seed)
      l = toList v
      l' = Data.List.take (2*size+1) l
  in zipWith (,) [0..((length l')-1)] l'

-- check whether the tree is consistent, i.e., if on has True, then one has (2k, (2p+1, True))
checkNode :: (Int, (Int,Bool)) -> Bool
checkNode (i,(j,b)) = (even i && odd j && b) || odd i || even j || not b

ok ::  [(Int, (Int, Bool))] -> Bool
ok = all checkNode 
