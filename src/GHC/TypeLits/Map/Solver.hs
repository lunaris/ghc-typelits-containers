{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.TypeLits.Map.Solver
  ( plugin
  ) where

--import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
--import Data.Maybe
--import DataCon
import FastString
import GHC.TcPluginM.Extra
import GhcPlugins           hiding (substTy)
--import TcEvidence
import TcPluginM            hiding (newWanted)
import TcRnTypes
import TyCoRep
--import Unify

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
      { _peMaps          :: IORef (M.Map TyCon (M.Map TyCon TyCon))
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
pluginSolve _ _gs _ds _ws = do
  tcPluginTrace "pluginSolve: " $ ppr (_gs, _ds, _ws)
  pure (TcPluginOk [] [])

simplify :: PluginEnv -> Type -> TcPluginM (Maybe ())
simplify env@PluginEnv{..}
  = go
  where
    go
      = \case
          TyConApp tyCon [k, m]
            | tyCon == _peLookupTyCon -> do
                tcPluginTrace "simplify: Lookup: " $ ppr (tyCon, k, m)
                mV <- reduceMap env m
                pure $ Just ()

reduceMap :: PluginEnv -> Type -> TcPluginM (Maybe (M.Map TyCon TyCon))
reduceMap PluginEnv{..}
  = go
  where
    go
      = \case
          TyConApp tyCon [as]
            | tyCon == _peFromListTyCon -> do
                tcPluginTrace "reduceMap: FromList: " $ ppr (tyCon, as)
                pure $ Just $ M.empty
          _ ->
            pure $ Nothing
