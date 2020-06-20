{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}           -- for Nat (lifting integer to type)
{-# LANGUAGE KindSignatures #-}      -- for (n :: Nat) declaration
{-# LANGUAGE RankNTypes #-}          -- for forall

module Vault where

import Polynomial
import Field
import qualified Prelude      as P
import qualified GHC.TypeLits as TL
import System.Random(Random,randomRs,newStdGen)
import Prelude(Int,Show,IO,Maybe(..),(<$>),(++),($),(.),(!!),fst,map,max,return,splitAt,
               take,zip,otherwise,toInteger,head)

import Data.List(nub)
import Data.Numbers.Primes(isPrime)

import Data.Proxy (Proxy(..))

-- Currently have MyDoubleon the right hand side but should be a
data Vault a = Vault { threshold :: Int, -- Number of points needed for threshold
                       shares    :: Int, -- Total number of shares that can be generated
                       secret    :: a    -- The secret member offinite field
                     } deriving (Show)

data Share a = Share { info  :: a,
                       order :: Int
                     } deriving (Show)

-- take list of values and puts them into a list of shares
--inShare :: [(Int,Int)] -> Int -> [Share]
inShare lst order' = map (\(x, y) -> Share { info = (x, y), order = order' }) lst

randomlist :: Random a => a -> a -> IO [a]
randomlist a b = randomRs (a, b) <$> newStdGen

--gets the order of the random field used dependent on the secret
ffOrder (Vault threshold shares secret') = nextPrime $ max secret' shares

nextPrime :: Int -> Int
nextPrime n | isPrime n = n   -- don't need to compare to True here
            | otherwise = nextPrime (n P.+1)

--generateShare :: (Field a, FField a) => Vault a -> IO [(a, a)]
generateShare vault@(Vault threshold shares secret') = do
         --let secret' = 5
         let Just someNat = TL.someNatVal (toInteger (ffOrder vault))

         case someNat of
             TL.SomeNat (_ :: Proxy n1) -> do
                 -- do stuff here with the computation
                 let secret = Modp secret' :: Modp n1

                 randoml <- randomlist 1 (ffOrder vault)
                 -- This vector can't have any duplicates otherwise the algorithm will fail
                 let randoml'   = map fromInteger randoml
                     -- this should be take unique
                     -- nub removes duplicate elements from a list
                     xvals      = take shares (nub randoml')
                     polynomial = Polynomial (secret : take (threshold P.-1) randoml')

                 let points :: [(Int,Int)] = zip (map unMod xvals) (map (unMod . evaluate polynomial) xvals)
                 return $ inShare points (ffOrder vault)

--decrypt :: [Share] -> IO( P.Int)
decrypt shares = do
         -- do a check to ensure they all have the same order
         let Just someNat = TL.someNatVal (toInteger (order (head shares)))
         case someNat of
             TL.SomeNat (_ :: Proxy n1) -> do
                 -- do stuff here with the computation
                 let x = map (toMod . P.fst . info) shares :: [Modp n1]
                 let y = map (toMod . P.snd . info) shares :: [Modp n1]
                 return P.$ unMod (coeff 1 x y)
