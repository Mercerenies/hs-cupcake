{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Graphics.Framing.Widget(Widget, newWidget, makeWidget, widgetOwner, text, position, visibility) where

import Graphics.Runtime.Identifier
import Graphics.Runtime.Environment
import Graphics.Windows.Control
import Graphics.Windows.Rect
import Graphics.Framing.Core
import Control.Applicative
import Control.Monad.State

makeWidget :: (Functor m, MonadState (RuntimeSetup r s) m) => CtrlType -> Frame -> m Widget
makeWidget ctype par = do
  widget <- newWidget ctype par <$> produceId
  rsp <- get
  put $ rsp { rspWidgets = widget : rspWidgets rsp }
  return widget

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
