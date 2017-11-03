{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Bag where

import           Control.Monad
import qualified Data.Aeson            as Ae
import           Data.Dynamic
import           Data.Functor.Identity
import           Data.Kind
import qualified Data.Map.Strict       as M
import           Data.Proxy
import qualified Data.Text             as Tx
import           GHC.TypeLits
import qualified GHC.TypeLits.Map      as TM
import qualified Language.Haskell.TH   as TH
import           Type.Reflection

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
  :: forall tag f.
     AllValues tag Ae.ToJSON f
  => Bag f (AllValuesMap tag)
  -> Ae.Value

bagToJSON (Bag m)
  = Ae.toJSON $ M.mapWithKey h m
  where
    dm = allValueDicts @tag @Ae.ToJSON @f

    h :: Tx.Text -> Dynamic -> Ae.Value
    h k (Dynamic tyRep x)
      = case M.lookup k dm of
          Just (SomeTypeableDict tyRep')
            | Just HRefl <- tyRep `eqTypeRep` tyRep' ->
                Ae.toJSON x

          _ ->
            error "bagToJSON: Impossible"

bagFromJSON
  :: forall tag f.
     AllValues tag Ae.FromJSON f
  => Ae.Value
  -> Maybe (Bag f (AllValuesMap tag))

bagFromJSON
  = fmap Bag . resultToMaybe . (M.traverseWithKey h <=< Ae.fromJSON)
  where
    dm = allValueDicts @tag @Ae.FromJSON @f

    h :: Tx.Text -> Ae.Value -> Ae.Result Dynamic
    h k v
      = case M.lookup k dm of
          Just (SomeTypeableDict (tyRep :: TypeRep a)) ->
            Dynamic tyRep <$> Ae.fromJSON @a v

          _ ->
            error "bagFromJSON: Impossible"

resultToMaybe :: Ae.Result a -> Maybe a
resultToMaybe
  = \case
      Ae.Error _   -> Nothing
      Ae.Success x -> Just x

data SomeTypeableDict :: (Type -> Constraint) -> Type where
  SomeTypeableDict :: c a => TypeRep a -> SomeTypeableDict c

type family AllValuesMap (tag :: Type) :: TM.Map Symbol Type

class AllValues (tag :: Type) (c :: Type -> Constraint) (f :: Type -> Type) where
  allValueDicts :: M.Map Tx.Text (SomeTypeableDict c)

instanceAllValues
  :: TH.Name
  -> TH.Name
  -> TH.Name
  -> TH.Name
  -> TH.DecsQ

instanceAllValues tagName ctName fName mName
  = let tagTy = TH.conT tagName
        ctTy  = TH.conT ctName
        fTy   = TH.conT fName

    in  [d| instance AllValues $tagTy $ctTy $fTy where
              allValueDicts = $(allValueDictsExpQ ctName fName mName)
          |]

allValueDictsExpQ :: TH.Name -> TH.Name -> TH.Name -> TH.ExpQ
allValueDictsExpQ ctName fName mName
  = TH.reify mName >>= \case
      TH.TyConI (TH.TySynD _name _tyVars mTy) ->
        case assocsToMap ctName fName <$> reifyAssocs mTy of
          Nothing ->
            fail "Unfooable Nothing"

          Just expQ ->
            expQ

      _ ->
        fail "Unfooable fail"

assocsToMap :: TH.Name -> TH.Name -> [(String, TH.Type)] -> TH.ExpQ
assocsToMap ctName fName
  = foldr insertAssoc [| M.empty |]
  where
    ctTy = TH.conT ctName
    fTy  = TH.conT fName

    insertAssoc :: (String, TH.Type) -> TH.ExpQ -> TH.ExpQ
    insertAssoc (k, vTy) mExpQ
      = [| M.insert (Tx.pack $(TH.stringE k))
            (SomeTypeableDict (typeRep :: TypeRep ($fTy $(pure vTy))) ::
              SomeTypeableDict $ctTy) $mExpQ

        |]

reifyAssocs :: TH.Type -> Maybe [(String, TH.Type)]
reifyAssocs
  = toList . removeSig . removeFromList . removeSig
  where
    removeSig :: TH.Type -> TH.Type
    removeSig
      = \case
          TH.SigT ty _kind -> ty
          ty               -> ty

    removeFromList :: TH.Type -> TH.Type
    removeFromList
      = \case
          TH.AppT (TH.ConT n) ty
            | n == ''TM.FromList -> ty
          ty ->
            ty

    toList :: TH.Type -> Maybe [(String, TH.Type)]
    toList
      = \case
          TH.AppT (TH.AppT TH.PromotedConsT ty1) ty2
            | TH.AppT (TH.AppT (TH.PromotedTupleT 2) (TH.LitT (TH.StrTyLit k))) vTy <-
                removeSig ty1 ->

                (:) (k, vTy) <$> toList (removeSig ty2)

          TH.PromotedNilT ->
            Just []

          _ ->
            Nothing
