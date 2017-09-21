{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module GHC.TypeLits.Map.Solver
  ( plugin
  ) where

--import Control.Monad.Reader
import Control.Arrow ((***), second)
import Data.IORef
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
      { _peMaps          :: IORef (M.Map TyVar (M.Map TyCon TyCon))
      , _peMapTyCon      :: TyCon
      , _peFromListTyCon :: TyCon
      , _peLookupTyCon   :: TyCon
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

  maps <- tcPluginIO $ newIORef mempty

  pure PluginEnv
    { _peMaps          = maps
    , _peMapTyCon      = mapTyCon
    , _peFromListTyCon = fromListTyCon
    , _peLookupTyCon   = lkTyCon
    }

pluginSolve :: PluginEnv -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
pluginSolve _ _gs [] []
  = pure (TcPluginOk [] [])
pluginSolve env gs ds ws = do
  tcPluginTrace "pluginSolve: " $ ppr (gs, ds, ws)
  let subst = collectTCvSubst (gs ++ ds)
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
      _ ->
        Just ty

toMapTypes
  :: PluginEnv
  -> TCvSubst
  -> Ct
  -> TcPluginM (Maybe (Ct, (MapType,MapType)))

toMapTypes env subst ct
  = case classifyPredType (ctEvPred (ctEvidence ct)) of
      EqPred NomEq t1 t2 -> do
        mc1 <- toMapType env (substTy subst t1)
        mc2 <- toMapType env (substTy subst t2)
        tcPluginTrace "MAPTRACE MAPTRACE: " $ ppr (mc1, mc2)
        pure (fmap (ct, ) $ (,) <$> mc1 <*> mc2)
      _ ->
        pure Nothing

toMapType :: PluginEnv -> Type -> TcPluginM (Maybe MapType)
toMapType env@PluginEnv{..}
  = go
  where
    go ty
      = case ty of
          TyConApp tyCon [_kKind, _vKind, asTy]
            | tyCon == _peFromListTyCon -> do
                as <- toMap env asTy
                tcPluginTrace "simplify: FromList: " $ ppr (tyCon, as)
                pure (FromListTy <$> as)
          TyConApp tyCon [_kKind, _vKind, kTy, mTy]
            | tyCon == _peLookupTyCon -> do
                tcPluginTrace "simplify: Lookup: " $ ppr (tyCon, kTy, mTy)
                kMTy <- go kTy
                mMTy <- go mTy
                pure (LookupTy <$> kMTy <*> mMTy)
          TyConApp tyCon tys
            | otherwise -> do
                tcPluginTrace "simplify: SOME TYCON: " $ ppr (tyCon, tyConName tyCon, tys)
                pure $ Just $ TypeTy ty
          _ ->
            pure $ Just $ TypeTy ty

toMap :: PluginEnv -> Type -> TcPluginM (Maybe ((Kind, Kind), M.Map OrdType Type))
toMap PluginEnv{..}
  = fmap (fmap (fmap M.fromList)) . go
  where
    go ty
      = case ty of
          TyConApp tyCon1 [TyConApp tyCon2 [kKind, vKind]]
            | tyCon1 == promotedNilDataCon -> do
              --tyCon2 == promotedTupleDataCon Boxed 2 -> do
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
