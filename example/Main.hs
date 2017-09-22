{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Map.Solver #-}

module Main where

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
  :: Proxy (Lookup "A1" (FromList
     '[ '("A1", 1)
      , '("A2", 2)
      ]))
  -> Proxy ('Just 1)

testLookupFromList
  = id

testLookupFromEmptyList
  :: Proxy (Lookup "A" (FromList '[]))
  -> Proxy 'Nothing

testLookupFromEmptyList
  = id

type family F a :: Maybe *

type instance F Int = 'Just Char

{-
testIgnoreOtherConstraints
  :: Proxy (F Bool)
  -> Proxy (F Char)

testIgnoreOtherConstraints
  = id
  -}

testFamilyWithLookup
  :: Proxy (F Int)
  -> Proxy (Lookup "A" (FromList '[ '("A", Char)]))

testFamilyWithLookup
  = id

main :: IO ()
main
  = pure ()
