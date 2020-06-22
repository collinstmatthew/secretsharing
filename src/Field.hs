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

class Field a => FField a where
    fromInteger :: Int -> a
    toInteger   :: a   -> Int
    size        :: a   -> Int

--F.Modp :: ghc-prim-0.5.3:GHC.Types.Nat -> *
--Hence Modp takes a type level Integer and gives back a concrete type
--I need a kind class which says instances must have a type constructor of this
--Form
newtype Modp (n :: TL.Nat) = Modpt Int deriving (Show)



instance forall n.(TL.KnownNat n) =>  Data.Eq.Eq (Modp n) where
  x == y = toInteger x  Data.Eq.== toInteger y

instance forall n.(TL.KnownNat n) => Field (Modp n) where
  zero    = fromInteger 0
  one     = fromInteger 1
  (+) x y = fromInteger (toInteger x P.+ toInteger y)
  (-) x y = fromInteger (toInteger x P.- toInteger y)
  (*) x y = fromInteger (toInteger x P.* toInteger y)
  (/) x y = fromInteger (toInteger x P.* snd (extendedEu (size x) (toInteger y)) )

instance forall n.(TL.KnownNat n) => FField (Modp n) where
    fromInteger x      = Modpt P.$ x `mod` P.fromInteger (TL.natVal (Proxy :: Proxy n))
    toInteger (Modpt i) = i
    size base@(Modpt s) = P.fromIntegral P.$ TL.natVal base

extendedEu :: Int -> Int -> (Int, Int)
extendedEu a 0 = (1, 0)
extendedEu a b = (t, s P.- q P.* t) where
          (q, r) = quotRem a b
          (s, t) = extendedEu b r

-- Computes x to the power of n for a field
power :: Field a => a -> Int -> a
power x 0 = one
power x n = iterate (x*) x !! (n P.- (1 :: Int))
