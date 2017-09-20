{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.TypeLits.Map
  ( Map
  , FromList
  ) where

data Map k v

type family FromList (as :: [(k, v)]) :: Map k v where
  FromList as = FromList as
