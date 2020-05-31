--https://crypto.stackexchange.com/questions/8240/implementing-secret-reconstruction-in-shamirs-secret-sharing/8262#8262

--https://rkrishnan.org/posts/2017-06-20-typesafe-modulus-in-haskell.html

module Main where

import Field
import Polynomial
import Vault

import Prelude(IO,($),fst,map,print,snd)

-- could dynamic distpatch to use other types

main :: IO()
main = do

  let mysecret = 1323821472
  let v = Vault {threshold = 1, shares = 1, secret = mysecret}

  --create order fuction which operates on Vault and gets the next prime number after mysecret
  -- check the s0 functinon is working and also Intl
  s <- generateShare v
  let order = ffOrder v
  let x = map (fst) s
  let y = map (snd) s
  result <- decrypt x y order
  print "results"
  print x
  print y
  print result
  print order
