{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Map.Solver #-}

module Main (main) where

import Data.Proxy
import GHC.TypeLits.Map

{-
testLookupFromListEquality
  :: (m ~ FromList '[ '("A", 1), '("B", 2)])
  => Proxy (Lookup "A" m)
  -> Proxy ('Just 1)

testLookupFromListEquality
  = id
  -}

testLookupFromList
  :: Proxy (Lookup "A" (FromList '[ '("A", 1), '("B", 2)]))
  -> Proxy ('Just 1)

testLookupFromList
  = id

testLookupFromEmptyList
  :: Proxy (Lookup "A" (FromList '[]))
  -> Proxy 'Nothing

testLookupFromEmptyList
  = id

main :: IO ()
main
  = pure ()
