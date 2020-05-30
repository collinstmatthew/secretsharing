{-# LANGUAGE DataKinds #-}           -- for Nat (lifting integer to type)

--https://crypto.stackexchange.com/questions/8240/implementing-secret-reconstruction-in-shamirs-secret-sharing/8262#8262

--https://rkrishnan.org/posts/2017-06-20-typesafe-modulus-in-haskell.html

module Main where

import Field
import Polynomial
import Vault

import Prelude(IO,fst,map,print,snd)

main :: IO()
main = do

  -- because no dependent types in haskell allow user to specify there own finite field
  -- but they must also specify the size
  --
  -- If there secret is too big to encode in a single number
  -- make the polynomial larger and give each person more shares
  --
  --Then do a comparison of size and computational stuff
  -- Here create secrete over a specific finite field
  let mysecret = Modp 12 :: Modp 7919
      v        = Vault {threshold = 20, shares = 20, secret = mysecret}

  s <- generateShare v

  let x = map fst s
  let y = map snd s

  print "The secret is:"
  print (decrypt x y)
