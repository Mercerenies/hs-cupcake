{-# LANGUAGE DataKinds, FlexibleContexts, MultiParamTypeClasses #-}

module Graphics.Framing.Widget(Widget, newWidget, makeWidget, makeButton, widgetOwner,
                               text, position, visibility, onClick) where

import Graphics.Reactive.Signal
import Graphics.Runtime.Identifier
import Graphics.Runtime.Environment
import Graphics.Runtime.Framework
import Graphics.Windows.Control hiding (makeButton)
import Graphics.Windows.Rect
import Graphics.Framing.Core
import Graphics.Message.Decode
import Graphics.Types.Tagged
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Debug.Trace

makeWidget :: (MonadState (RuntimeSetup r s) m) => CtrlType -> Frame -> m Widget
makeWidget ctype par = do
  widget <- newWidget ctype par <$> produceId
  rsp <- get
  put $ rsp { rspWidgets = widget : rspWidgets rsp }
  return widget

makeButton :: (MonadState (RuntimeSetup r s) m) => Frame -> m (Tagged 'Button Widget)
makeButton par = tag' <$> makeWidget Button par

text :: Widget -> String -> EnvReader r s ()
text wdgt str = do
  ctrl <- magicCast wdgt
  liftIO $ setCtrlText ctrl str

position :: Widget -> Rect -> EnvReader r s ()
position wdgt rect = do
  ctrl <- magicCast wdgt
  liftIO $ setCtrlPos ctrl rect

visibility :: Widget -> Bool -> EnvReader r s ()
visibility wdgt bool = do
  ctrl <- magicCast wdgt
  liftIO $ if bool then showCtrl ctrl else hideCtrl ctrl

onClick :: (MonadReader (RuntimeSystem r s) m) => Tagged 'Button Widget -> m (SignalT r (EnvReader r s) ())
onClick wdgt = do
  click0 <- asks (Map.lookup TButtonEvent . rsEvents)
  return $ case click0 of
             Nothing -> killSignal
             Just (click, _) -> hairpin click ~~> \cev -> do
                                  env <- ask
                                  case cev of
                                    Just (ButtonEvent ctrl)
                                        | untag wdgt == envMagicCast env (untag ctrl) -> pure ()
                                        | otherwise -> mzero
                                    _ -> mzero
