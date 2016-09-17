{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Graphics.Runtime.Environment(RuntimeSetup(..), RuntimeEnv(..), EnvReader, Frame, Widget,
                                    runEnvReader, runtimeSetup, setupEnv, EnvMagicCast(..), magicCast,
                                    rspUpdateId) where

import Graphics.Windows.CoreTypes
import Graphics.Windows.Control
import Graphics.Windows.Window
import Graphics.Framing.Core
import Graphics.Runtime.Identifier
import Graphics.Types.Tagged
import Control.Monad.Trans.Maybe
import Control.Monad.RWS
import qualified Data.Map as Map

data RuntimeSetup r s = RuntimeSetup {
      rspId :: Integer,
      rspFrames :: [Frame],
      rspWidgets :: [Widget]
    }

data RuntimeEnv r = RuntimeEnv {
      frameToHandle :: Frame -> Handle,
      handleToFrame :: Handle -> Frame,
      widgetToCtrl :: Widget -> Control,
      ctrlToWidget :: Control -> Widget
    }

type EnvReader r s = MaybeT (RWST (RuntimeEnv r) () s IO)

instance IdEnv (RuntimeSetup r s) Integer where
    makeId rs = let curr = rspId rs
                in (curr, rs { rspId = curr })

rspUpdateId :: MonadState (RuntimeSetup r s) m => m Integer
rspUpdateId = do
  rsp <- get
  put $ rsp { rspId = rspId rsp + 1 }
  return $ rspId rsp

runEnvReader :: EnvReader r s a -> RuntimeEnv r -> s -> IO (Maybe a, s, ())
runEnvReader env = runRWST (runMaybeT env)

runtimeSetup :: RuntimeSetup r s
runtimeSetup = RuntimeSetup {
                   rspId = 0,
                   rspFrames = [],
                   rspWidgets = []
                 }

setupEnv :: RuntimeSetup r s -> IO (RuntimeEnv r)
setupEnv rsp = do
  let lookupFunc m x = Map.findWithDefault (error "invalid id state") x m -- TODO Handle error
  map0 <- mapM (const $ makeWindow "") (rspFrames rsp)
  let map1 = Map.fromList $ zip (rspFrames rsp) map0
      map1' = Map.fromList $ zip map0 (rspFrames rsp)
  mapw0 <- mapM (\wdgt -> makeCtrl "" (getTag wdgt) $ lookupFunc map1 (widgetOwner wdgt)) (rspWidgets rsp)
  let mapw = Map.fromList $ zip (rspWidgets rsp) mapw0
      mapw' = Map.fromList $ zip mapw0 (rspWidgets rsp)
  return $ RuntimeEnv {
                 frameToHandle = lookupFunc map1,
                 handleToFrame = lookupFunc map1',
                 widgetToCtrl = lookupFunc mapw,
                 ctrlToWidget = lookupFunc mapw'
               }

class EnvMagicCast a b where
    envMagicCast :: RuntimeEnv r -> a -> b

instance EnvMagicCast Frame Handle where
    envMagicCast = frameToHandle

instance EnvMagicCast Handle Frame where
    envMagicCast = handleToFrame

instance EnvMagicCast Widget Control where
    envMagicCast = widgetToCtrl

instance EnvMagicCast Control Widget where
    envMagicCast = ctrlToWidget

magicCast :: EnvMagicCast a b => a -> EnvReader r s b
magicCast x = asks (\z -> envMagicCast z x)
