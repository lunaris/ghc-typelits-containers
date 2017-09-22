{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Map.Solver #-}

module Main where

import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Map

testHasKey
  :: HasKey m k v
  => ()

testHasKey
  = ()

{-
testNested1
  :: (m ~ FromList '[ '("A1", "A1")])
  => Proxy (FromList '[ '("A1", 1) ])
  -> Proxy (FromList '[ '(Lookup "A1" m, 1) ])

testNested1
  = id
  -}

{-
testNested2
  :: (m ~ FromList '[ '("A1", "A1")])
  => Proxy (FromList '[ '("A1", 1) ])
  -> Proxy (FromList '[ '("A1", Lookup "A1" m) ])

testNested2
  = id
  -}

testInert
  :: Proxy (FromList '[ '("A1", 1) ])
  -> Proxy (FromList '[ '("A1", 1) ])

testInert
  = id

testLookupAllFromList
  :: Proxy (LookupAll '["A1", "A2", "A3"]
      (FromList '[ '("A1", 1), '("A2", 2), '("A3", 3), '("A4", 4)]))
  -> Proxy ('Just '[1, 2, 3])

testLookupAllFromList
  = id

testLookupFromListEquality
  :: (a ~ '("AE", 1),
      m ~ FromList '[ a, '("BE", 2)])
  => Proxy (Lookup "AE" m)
  -> Proxy ('Just 1)

testLookupFromListEquality
  = id

type M
  = FromList '[ '("AM", 1), '("BM", 2) ]

testLookupFromListTySyn
  :: Proxy (Lookup "AM" M)
  -> Proxy ('Just 1)

testLookupFromListTySyn
  = id

testLookupFromList
  :: Proxy (Lookup "A1" (FromList
     '[ '("A1", 1)
      , '("A2", 2)
      ]))
  -> Proxy ('Just 1)

testLookupFromList
  = id

testLookupFromEmptyList
  :: Proxy (Lookup "A" (FromList ('[] :: [(Symbol, Nat)])))
  -> Proxy ('Nothing :: Maybe Nat)

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
