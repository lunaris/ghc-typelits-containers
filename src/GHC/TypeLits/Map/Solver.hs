{-# LANGUAGE RecordWildCards #-}

module GHC.TypeLits.Map.Solver
  ( plugin
  ) where

import           GHC.TypeLits.Map.Solver.Env
import qualified GHC.TypeLits.Map.Solver.GHC        as GHC
import qualified GHC.TypeLits.Map.Solver.Meta       as Meta
import           GHC.TypeLits.Map.Solver.Operations

import           Control.Arrow                      ((***), second)
import qualified Control.Monad.Trans                as Trans
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (catMaybes)

plugin :: GHC.Plugin
plugin
  = GHC.defaultPlugin
      { GHC.tcPlugin = const (Just plugin')
      }

plugin' :: GHC.TcPlugin
plugin'
  = GHC.tracePlugin Meta.packageName GHC.TcPlugin
      { tcPluginInit  = pluginInit
      , tcPluginSolve = pluginSolve
      , tcPluginStop  = const (pure ())
      }

pluginInit :: GHC.TcPluginM PluginEnv
pluginInit = do
  let pkgName    = GHC.fsLit Meta.packageName
      apiModName = GHC.mkModuleName Meta.apiModuleName

  apiMod <- GHC.lookupModule apiModName pkgName
  let lookupMapTyCon name
        = GHC.lookupName apiMod (GHC.mkTcOcc name) >>= GHC.tcLookupTyCon

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

pluginSolve
  :: PluginEnv
  -> [GHC.Ct]
  -> [GHC.Ct]
  -> [GHC.Ct]
  -> GHC.TcPluginM GHC.TcPluginResult

pluginSolve _ _gs [] []
  = pure (GHC.TcPluginOk [] [])
pluginSolve env gs ds ws = runPluginM env $ do
  pluginTrace "pluginSolve: Entry" (gs, ds, ws)
  zgs <- Trans.lift $ traverse GHC.zonkCt gs
  pluginTrace "pluginSolve: Zonked givens" zgs
  let sub = GHC.collectEqCtSubsts (zgs ++ ds)
  pluginTrace "pluginSolve: Collected substitution" sub
  ops <- catMaybes <$> traverse (eqPredMapOps sub) ws
  let reducedOps = map (second (reduceOp *** reduceOp)) ops
  pluginTrace "pluginSolve: Reduced operations" reducedOps
  (solved, newWs) <- unzip <$> traverse solvedOpsEqPred reducedOps
  pluginTrace "pluginSolve: Rebuilt solved predicates" solved
  pure (GHC.TcPluginOk solved newWs)

reduceOp :: MapOp -> MapOp
reduceOp op
  = case op of
      LookupOp (TypeOp kt) (FromListOp ((_kk, vk), m)) ->
        case M.lookup (OrdType kt) m of
          Nothing ->
            TypeOp $ GHC.mkTyConApp GHC.promotedNothingDataCon [vk]
          Just vt ->
            TypeOp $ GHC.mkTyConApp GHC.promotedJustDataCon [vk, vt]
      LookupAllOp (_kk1, ksl) (FromListOp ((_kk2, vk), m)) ->
        case traverse (\kt -> M.lookup (OrdType kt) m) ksl of
          Nothing ->
            TypeOp $ GHC.mkTyConApp GHC.promotedNothingDataCon [vk]
          Just vts ->
            TypeOp $ GHC.mkTyConApp GHC.promotedJustDataCon
              [ GHC.mkTyConApp GHC.listTyCon [vk]
              , GHC.mkPromotedListTy vk vts
              ]
      _ ->
        op
