{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}           -- for Nat (lifting integer to type)
{-# LANGUAGE KindSignatures #-}      -- for (n :: Nat) declaration
{-# LANGUAGE RankNTypes #-}          -- for forall

module Vault where

import qualified Prelude as P
import Polynomial
import Field
import System.Random
import Prelude(Int,Show,IO,(<$>),(++),($),fst,map,return,splitAt,take,zip, (!!),print)
import Data.List(nub)

import qualified GHC.TypeLits as TL
import Data.Proxy (Proxy(..))

import Data.Numbers.Primes(isPrime)

-- Currently have MyDoubleon the right hand side but should be a
data Vault a = Vault {  threshold :: Int, -- Number of points needed for threshold
                        shares    :: Int, -- Total number of shares that can be generated
                        secret    :: a    -- The secret member offinite field
                      } deriving (Show)

randomlist :: Random a => a -> a -> IO [a]
randomlist a b = randomRs (a, b) <$> newStdGen

--gets the order of the random field used dependent on the secret
ffOrder (Vault threshold shares secret') = nextPrime P.$ P.max secret' shares

nextPrime :: Int -> Int
nextPrime n | isPrime n = n   -- don't need to compare to True here
            | P.otherwise = nextPrime (n P.+1)

--generateShare :: (Field a, FField a) => Vault a -> IO [(a, a)]
generateShare vault@(Vault threshold shares secret') = do
                     --let secret' = 5
                     let P.Just someNat = TL.someNatVal (P.toInteger (ffOrder vault))
                     case someNat of
                         TL.SomeNat (_ :: Proxy n1) -> do
                             -- do stuff here with the computation
                             --let x = Modp secret' :: Modp n1
                             let secret = Modp secret' :: Modp n1
                             randoml <- randomlist 1 (ffOrder vault)
                             -- This vector can't have any duplicates otherwise the algorithm will fail
                             let randoml'   = map fromInteger randoml
                                 -- this should be take unique
                                 -- nub removes duplicate elements from a list
                                 xvals      = take shares (nub randoml')
                                 polynomial = Polynomial (secret : P.take (threshold P.-1) randoml')
                             return P.$ (zip (map unMod xvals) (map unMod (map (evaluate polynomial) xvals)))

-- Encrypt function
--encrypt :: Field f => (a -> f) -> a -> f
-- make this dynamic depending on the size of x
--encrypt x = Modp x :: Modp ( x)

--decrypt :: Field a => [a] -> [a] -> a ->IO( P.Integer)
--decrypt x y = s0 x y
decrypt x y order = do
         let P.Just someNat = TL.someNatVal (P.toInteger order)
         case someNat of
             TL.SomeNat (_ :: Proxy n1) -> do
                 -- do stuff here with the computation
                 --let x = Modp secret' :: Modp n1
                 let x1 = map toMod x :: [Modp n1]
                 let y1 = map toMod y :: [Modp n1]
                 let result = s0 x1 y1
                 print y1
                 print result
                 print P.$ unMod result
                 return P.$ unMod result
