module Field where

import qualified Prelude as P
import Prelude(($), (!!), (++), (.), Int, Show, iterate, mod, quotRem, snd)

import Data.Eq

class Eq a => Field a where
  zero :: a
  one  :: a
  (-)  :: a -> a -> a
  (+)  :: a -> a -> a
  (/)  :: a -> a -> a
  (*)  :: a -> a -> a

class FField a where
    fromInteger :: Int -> a
    size        :: a   -> Int

base :: Int
base = 7919

newtype Modp = Modp Int deriving (Show, Eq)
instance Field Modp where
  zero = Modp 0
  one  = Modp 1
  (+) (Modp x) (Modp y) = Modp $ mod (x P.+ y) base
  (-) (Modp x) (Modp y) = Modp $ mod (x P.- y) base
  (*) (Modp x) (Modp y) = Modp $ mod (x P.* y) base
  (/) (Modp x) (Modp y) = Modp $ mod (x P.* snd (extendedEu base y)) base

instance FField Modp where
    fromInteger x = Modp $ mod x base
    size        x = base

extendedEu :: Int -> Int -> (Int, Int)
extendedEu a 0 = (1, 0)
extendedEu a b = (t, s P.- q P.* t) where
          (q, r) = quotRem a b
          (s, t) = extendedEu b r

-- Computes x to the power of n for a field
power :: Field a => a -> Int -> a
power x 0 = one
power x n = iterate (x *) x !! (n P.- 1)
