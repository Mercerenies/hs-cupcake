{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Graphics.Framing.Frame(Frame, newFrame, makeFrame, titlebar, visibility) where

import Graphics.Runtime.Identifier
import Graphics.Runtime.Environment
import Graphics.Framing.Core
import Graphics.Windows.Window
import Control.Applicative
import Control.Monad.State

makeFrame :: (Functor m, MonadState (RuntimeSetup r s) m) => m Frame
makeFrame = do
  frame <- newFrame <$> produceId
  rsp <- get
  put $ rsp { rspFrames = frame : rspFrames rsp }
  return frame

titlebar :: Frame -> String -> EnvReader r s ()
titlebar frame str = do
  hwnd <- magicCast frame
  liftIO $ setWindowTitle hwnd str

visibility :: Frame -> Bool -> EnvReader r s ()
visibility frame bool = do
  hwnd <- magicCast frame
  liftIO $ if bool then showWindow hwnd else hideWindow hwnd
