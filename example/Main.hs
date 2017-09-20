{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Map.Solver #-}

module Main (main) where

import Data.Proxy
import GHC.TypeLits.Map

testLookupFromList
  :: Proxy (Lookup "A" (FromList '[ '("A", 1), '("B", 2)]))
  -> Proxy ('Just 1)

testLookupFromList
  = id

main :: IO ()
main
  = pure ()
