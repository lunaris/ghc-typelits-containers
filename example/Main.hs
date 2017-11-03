{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Map.Solver #-}

module Main where

import qualified Bag
import           Types

import           Data.Functor.Identity
import           Data.Proxy
import           GHC.TypeLits
import           GHC.TypeLits.Map

b1, b2, b3, b4 :: Bag.Bag Identity BM

b1 = mempty
b2 = Bag.insert @"B10" (Identity 'c') b1
b3 = Bag.insert @"B192" (Identity "hello") b2
b4 = Bag.insert @"B365" (Identity 42) b3

testNonSymbolKeys
  :: Proxy (Lookup Int (FromList '[ '(Int, 1), '(Bool, 2)]))
  -> Proxy ('Just 1)

testNonSymbolKeys
  = id

testKeys
  :: Proxy (Keys (FromList '[ '("A", 1), '("B", 2)]))
  -> Proxy '["A", "B"]

testKeys
  = id

testElems
  :: Proxy (Elems (FromList '[ '("A", 1), '("B", 2)]))
  -> Proxy '[1, 2]

testElems
  = id

{-
testAssocs
  :: (m ~ AS,
      m1 ~ AS)
  => Proxy (Assocs (FromList m))
  -> Proxy (Assocs (FromList m1))

testAssocs
  = id
  -}

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

{-
testLookupFromListTySyn2
  :: Proxy (Lookup "A230" (FromList AS))
  -> Proxy ('Just 230)

testLookupFromListTySyn2
  = id

testLookupAllFromListTySyn
  :: Proxy (LookupAll '["A123", "A234", "A345", "A456", "A567", "A678"] (FromList AS))
  -> Proxy ('Just '[123, 234, 345, 456, 567, 678])

testLookupAllFromListTySyn
  = id

testLookupFromList
  :: Proxy (Lookup "A1" (FromList AS))
  -> Proxy ('Just 1)

testLookupFromList
  = id
  -}

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
main = do
  print $ Bag.bagToJSON @P b4
  putStrLn
    $ maybe "" (show . Bag.bagToJSON @P)
    $ Bag.bagFromJSON @P @Identity
    $ Bag.bagToJSON @P b4

  pure ()
