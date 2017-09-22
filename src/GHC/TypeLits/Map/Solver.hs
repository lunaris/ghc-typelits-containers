{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module GHC.TypeLits.Map.Solver
  ( plugin
  ) where

--import Control.Monad.Reader
import Control.Arrow ((***), second)
import qualified Data.Map.Strict as M
import Data.Maybe
--import DataCon
import FastString
import GHC.TcPluginM.Extra
import GhcPlugins           hiding (substTy)
import TcEvidence
import TcPluginM            hiding (newWanted)
import TcRnTypes
import TcType
import TyCoRep
import Unify

plugin :: Plugin
plugin
  = defaultPlugin
      { tcPlugin = const (Just plugin')
      }

plugin' :: TcPlugin
plugin'
  = tracePlugin "ghc-typelits-containers:Map" TcPlugin
      { tcPluginInit  = pluginInit
      , tcPluginSolve = pluginSolve
      , tcPluginStop  = const (pure ())
      }

data PluginEnv
  = PluginEnv
      { _peMapTyCon       :: TyCon
      , _peFromListTyCon  :: TyCon
      , _peLookupTyCon    :: TyCon
      , _peLookupAllTyCon :: TyCon
      }

pluginInit :: TcPluginM PluginEnv
pluginInit = do
  let pkgName    = fsLit "ghc-typelits-containers"
      mapModName = mkModuleName "GHC.TypeLits.Map"

  mapMod <- lookupModule mapModName pkgName
  let lookupMapTyCon name
        = lookupName mapMod (mkTcOcc name) >>= tcLookupTyCon

  mapTyCon <- lookupMapTyCon "Map"
  fromListTyCon <- lookupMapTyCon "FromList"
  lkTyCon <- lookupMapTyCon "Lookup"
  lookupAllTyCon <- lookupMapTyCon "LookupAll"

  pure PluginEnv
    { _peMapTyCon       = mapTyCon
    , _peFromListTyCon  = fromListTyCon
    , _peLookupTyCon    = lkTyCon
    , _peLookupAllTyCon = lookupAllTyCon
    }

pluginSolve :: PluginEnv -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
pluginSolve _ _gs [] []
  = pure (TcPluginOk [] [])
pluginSolve env gs ds ws = do
  tcPluginTrace "pluginSolve: " $ ppr (gs, ds, ws)
  zgs <- traverse zonkCt gs
  tcPluginTrace "MAPTRACE ZONKED: " $ ppr zgs
  let subst = collectTCvSubst (zgs ++ ds)
  tcPluginTrace "MAPTRACE SUBST: " $ ppr subst
  pluginWs <- catMaybes <$> traverse (toMapTypes env subst) ws
  let _xs = map (second (reduce *** reduce)) pluginWs
  tcPluginTrace "MAPTRACE REDUCE: " $ ppr _xs
  (_ys, _zs) <- unzip . catMaybes <$> traverse (reifyCt env) _xs
  tcPluginTrace "MAPTRACE REIFY: " $ ppr _ys
  pure (TcPluginOk _ys _zs)

collectTCvSubst :: [Ct] -> TCvSubst
collectTCvSubst
  = foldr (unionTCvSubst . k) emptyTCvSubst
  where
    k ct
      = case classifyPredType (ctPred ct) of
          EqPred NomEq t1 t2 ->
            fromMaybe emptyTCvSubst (tcUnifyTy t1 t2)
          _ ->
            emptyTCvSubst

data MapType
  = LookupTy MapType MapType
  | LookupAllTy (Kind, [Type]) MapType
  | FromListTy ((Kind, Kind), M.Map OrdType Type)
  | TypeTy Type

reifyCt :: PluginEnv -> (Ct, (Maybe MapType, Maybe MapType)) -> TcPluginM (Maybe ((EvTerm, Ct), Ct))
reifyCt env (ct, (mty1, mty2)) = do
  let mt1 = fmap (reifyMapType env) mty1
      mt2 = fmap (reifyMapType env) mty2

      k t1 t2 = do
        w <- newWanted (ctLoc ct) (mkPrimEqPred t1 t2)
        let wct = mkNonCanonical w
        pure ((evByFiat "ghc-typelits-containers" (ctPred ct) t1, ct), wct)

  sequence $ k <$> mt1 <*> mt2

reifyMapType :: PluginEnv -> MapType -> Type
reifyMapType env@PluginEnv{..}
  = \case
      LookupTy kMTy mMTy ->
        mkTyConApp _peLookupTyCon [reifyMapType env kMTy, reifyMapType env mMTy]
      LookupAllTy _ksMTy _mMTy ->
        error "reifyMapType: LookupAllTy"
      FromListTy _m ->
        error "reifyMapType: FromListTy"
      TypeTy ty ->
        ty

newtype OrdType
  = OrdType Type
  deriving (Outputable)

instance Eq OrdType where
  OrdType t1 == OrdType t2
    = eqType t1 t2

instance Ord OrdType where
  compare (OrdType t1) (OrdType t2)
    = cmpType t1 t2

instance Outputable MapType where
  ppr
    = parens . \case
        LookupTy kMTy mMTy ->
          text "LookupTy" <+> ppr kMTy <+> ppr mMTy
        LookupAllTy ksMTy mMTy ->
          text "LookupAllTy" <+> ppr ksMTy <+> ppr mMTy
        FromListTy mMTy ->
          text "FromListTy" <+> ppr mMTy
        TypeTy ty ->
          text "TypeTy" <+> ppr ty

reduce :: MapType -> Maybe MapType
reduce ty
  = case ty of
      LookupTy (TypeTy kTy) (FromListTy ((_kKind, vKind), m)) ->
        case M.lookup (OrdType kTy) m of
          Nothing ->
            Just $ TypeTy $ mkTyConApp promotedNothingDataCon [vKind]
          Just vTy ->
            Just $ TypeTy $ mkTyConApp promotedJustDataCon [vKind, vTy]
      LookupAllTy (_kKind1, ksTys) (FromListTy ((_kKind2, vKind), m)) ->
        case traverse (\kTy -> M.lookup (OrdType kTy) m) ksTys of
          Nothing ->
            Just $ TypeTy $ mkTyConApp promotedNothingDataCon [vKind]
          Just vTys ->
            Just $ TypeTy $ mkTyConApp promotedJustDataCon
              [ mkTyConApp listTyCon [vKind]
              , foldr (\a as -> mkTyConApp promotedConsDataCon [vKind, a, as])
                  (mkTyConApp promotedNilDataCon [vKind]) vTys

              ]
      _ ->
        Just ty

toMapTypes
  :: PluginEnv
  -> TCvSubst
  -> Ct
  -> TcPluginM (Maybe (Ct, (MapType, MapType)))

toMapTypes env subst ct
  = case classifyPredType (ctEvPred (ctEvidence ct)) of
      EqPred NomEq t1 t2 -> do
        mt1 <- toMapType env (substTy subst t1)
        mt2 <- toMapType env (substTy subst t2)
        tcPluginTrace "MAPTRACE MAPTRACE: " $ ppr (mt1, mt2)
        pure $ case (mt1, mt2) of
          (TypeTy _, TypeTy _) ->
            Nothing
          mts ->
            Just (ct, mts)
      _ ->
        pure Nothing

toMapType :: PluginEnv -> Type -> TcPluginM MapType
toMapType env@PluginEnv{..}
  = go
  where
    go ty
      = case ty of
          TyConApp tyCon [_kKind, _vKind, asTy]
            | tyCon == _peFromListTyCon -> do
                as <- toMap env asTy
                tcPluginTrace "simplify: FromList: " $ ppr (tyCon, as)
                pure (maybe (TypeTy ty) FromListTy as)
          TyConApp tyCon [_kKind, _vKind, kTy, mTy]
            | tyCon == _peLookupTyCon -> do
                tcPluginTrace "simplify: Lookup: " $ ppr (tyCon, kTy, mTy)
                kMTy <- go kTy
                mMTy <- go mTy
                pure (LookupTy kMTy mMTy)
          TyConApp tyCon [_ksKind, _vKind, ksTy, mTy]
            | tyCon == _peLookupAllTyCon -> do
                tcPluginTrace "simplify: LookupAll: " $ ppr (tyCon, ksTy, mTy)
                ksMTy <- toList ksTy
                mMTy <- go mTy
                pure (maybe (TypeTy ty) (\x -> LookupAllTy x mMTy) ksMTy)
          TyConApp tyCon tys
            | otherwise -> do
                tcPluginTrace "simplify: SOME TYCON: " $ ppr (tyCon, tyConName tyCon, tys)
                pure $ TypeTy ty
          _ ->
            pure $ TypeTy ty

toMap :: PluginEnv -> Type -> TcPluginM (Maybe ((Kind, Kind), M.Map OrdType Type))
toMap PluginEnv{..}
  = fmap (fmap (fmap M.fromList)) . go
  where
    go ty
      = case ty of
          TyConApp tyCon1 [TyConApp tyCon2 [kKind, vKind]]
            | tyCon1 == promotedNilDataCon -> do
                tcPluginTrace "toMap: NIL " $ ppr (tyCon2, kKind, vKind)
                pure $ Just ((kKind, vKind), [])
          TyConApp tyCon1 [_aKind, TyConApp tyCon2 [_kKind, _vKind, kTy, vTy], asTy]
            | tyCon1 == promotedConsDataCon &&
              tyCon2 == promotedTupleDataCon Boxed 2 -> do
                tcPluginTrace "toMap: CONS " $ ppr (kTy, vTy, asTy)
                as <- go asTy
                pure $ (fmap ((:) (OrdType kTy, vTy)) <$> as)
          _ ->
            pure Nothing

toList :: Type -> TcPluginM (Maybe (Kind, [Type]))
toList
  = go
  where
    go ty
      = case ty of
          TyConApp tyCon [aKind]
            | tyCon == promotedNilDataCon -> do
                tcPluginTrace "toList: NIL " $ ppr (tyCon, aKind)
                pure $ Just (aKind, [])
          TyConApp tyCon [_aKind, aTy, asTy]
            | tyCon == promotedConsDataCon -> do
                tcPluginTrace "toList: CONS " $ ppr (aTy, asTy)
                as <- go asTy
                pure $ (fmap (aTy :) <$> as)
          _ ->
            pure Nothing
