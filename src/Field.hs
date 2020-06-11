{-# LANGUAGE KindSignatures #-}      -- for (n :: Nat) declaration
{-# LANGUAGE DataKinds #-}           -- for Nat (lifting integer to type)
{-# LANGUAGE RankNTypes #-}          -- for forall
{-# LANGUAGE ScopedTypeVariables #-} -- when type signature is used inside a fn with a top level forall, we need this for scoping

module Field where

import qualified Prelude as P
import Prelude(($), (!!), (++), (.), Int, Show, iterate, mod, quotRem, snd)
import Data.Eq
import qualified GHC.TypeLits as TL
import Data.Proxy (Proxy(..))
import Data.TypeLevel hiding ((+), (-), (*), (/), Mod, mod)

-- Must be able to compareelementsofa field
class Data.Eq.Eq a => Field a where
  zero :: a
  one  :: a
  (-)  :: a -> a -> a
  (+)  :: a -> a -> a
  (/)  :: a -> a -> a
  (*)  :: a -> a -> a

class FField a where
    fromInteger :: Int -> a
    size        :: a   -> Int

newtype Modp (n :: TL.Nat) = Modp Int deriving (Show)

toMod :: forall n . ( TL.KnownNat n) => Int -> Modp n
toMod i = Modp P.$ i `mod` P.fromInteger (TL.natVal (Proxy :: Proxy n))

unMod :: Modp n -> Int
unMod (Modp i) = i

getBase :: TL.KnownNat n => Modp n -> Int
getBase base@(Modp s) = P.fromIntegral P.$ TL.natVal base


instance forall n.(TL.KnownNat n) =>  Data.Eq.Eq (Modp n) where
  x == y = unMod (x :: Modp n) Data.Eq.== unMod (y :: Modp n)

instance forall n.(TL.KnownNat n) => Field (Modp n) where
  zero    = toMod 0 :: Modp n
  one     = toMod 1 :: Modp n
  (+) x y =  toMod (unMod x P.+ unMod y) :: Modp n
  (-) x y =  toMod (unMod x P.- unMod y) :: Modp n
  (*) x y =  toMod (unMod x P.* unMod y) :: Modp n
  (/) x y =  toMod (unMod x P.* snd (extendedEu (getBase x) (unMod y)) ) :: Modp n

instance forall n.(TL.KnownNat n) => FField (Modp n) where
    fromInteger x = toMod x :: Modp n
    size          = getBase



extendedEu :: Int -> Int -> (Int, Int)
extendedEu a 0 = (1, 0)
extendedEu a b = (t, s P.- q P.* t) where
          (q, r) = quotRem a b
          (s, t) = extendedEu b r

-- Computes x to the power of n for a field
power :: Field a => a -> Int -> a
power x 0 = one
power x n = iterate (x*) x !! (n P.- (1 :: Int))
