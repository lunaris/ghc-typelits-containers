{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}

module Bag where

import qualified Data.Aeson            as Ae
import           Data.Dynamic
import           Data.Functor.Identity
import           Data.Kind
import qualified Data.Map.Strict       as M
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text             as Tx
import           GHC.TypeLits
import qualified GHC.TypeLits.Map      as TM
import           Type.Reflection

class AllF (c :: k2 -> Constraint) (f :: k1 -> k2) (as :: [k1]) where
  dictsAssocsF :: [(SomeTypeRep, SomeDictF c f)]

instance AllF c f '[] where
  dictsAssocsF
    = []

instance (Typeable (f a),
          c (f a),
          AllF c f as)

      =>  AllF c f (a ': as) where

  dictsAssocsF
    = let tr = typeRep @(f a)
      in  (SomeTypeRep tr, SomeDictF @_ @_ @c @f tr) : dictsAssocsF @_ @_ @c @f @as

data SomeDictF :: (k2 -> Constraint) -> (k1 -> k2) -> Type where
  SomeDictF :: c (f a) => TypeRep (f a) -> SomeDictF c f

dictsMapF
  :: forall c f as.
     AllF c f as
  => M.Map SomeTypeRep (SomeDictF c f)

dictsMapF
  = M.fromList (dictsAssocsF @_ @_ @c @f @as)

newtype Bag (f :: Type -> Type) (m :: TM.Map Symbol Type)
  = Bag { _bagMap :: M.Map Tx.Text Dynamic }

instance Monoid (Bag f m) where
  mempty
    = Bag mempty
  mappend (Bag m1) (Bag m2)
    = Bag (m1 `M.union` m2)

insert
  :: forall k v f m.
     (KnownSymbol k,
      Typeable (f v),
      TM.HasKey m k v)
  => f v
  -> Bag f m
  -> Bag f m

insert fv (Bag m)
  = Bag (M.insert (symbolText @k) (toDyn fv) m)

insertValue
  :: forall k v m.
     (KnownSymbol k,
      Typeable v,
      TM.HasKey m k v)
  => v
  -> Bag Identity m
  -> Bag Identity m

insertValue
  = insert @k . Identity

lookup
  :: forall k v f m.
     (KnownSymbol k,
      Typeable (f v),
      TM.HasKey m k v)
  => Bag f m
  -> Maybe (f v)

lookup (Bag m)
  = M.lookup (symbolText @k) m >>= fromDynamic

symbolText :: forall s. KnownSymbol s => Tx.Text
symbolText
  = Tx.pack (symbolVal (Proxy @s))

bagToJSON
  :: forall f m.
     AllF Ae.ToJSON f (TM.Elems m)
  => Bag f m
  -> Ae.Value

bagToJSON (Bag m)
  = Ae.toJSON (M.map k m)
  where
    dm
      = dictsMapF @Ae.ToJSON @f @(TM.Elems m)
    k :: Dynamic -> Ae.Value
    k (Dynamic tr1 x)
      = fromMaybe (error "bagToJSON: Impossible") $ do
          SomeDictF (tr2 :: TypeRep (f a)) <- M.lookup (SomeTypeRep tr1) dm
          case eqTypeRep tr1 tr2 of
            Nothing ->
              Nothing
            Just HRefl ->
              Just (Ae.toJSON x)
