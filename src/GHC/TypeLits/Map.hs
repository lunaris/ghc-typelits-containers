{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.TypeLits.Map
  ( Map
  , FromList
  , Lookup
  ) where

data Map key value

type family FromList (as :: [(key, value)]) :: Map key value

type family Lookup (k :: key) (m :: Map key value) :: Maybe value
