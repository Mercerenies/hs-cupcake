{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module Graphics.Framing.Frame(Frame, newFrame, makeFrame, titlebar, visibility, onClick) where

import Graphics.Reactive.Signal
import Graphics.Runtime.Identifier
import Graphics.Runtime.Environment
import Graphics.Runtime.Framework
import Graphics.Framing.Core
import Graphics.Windows.Window
import Graphics.Message.Decode
import qualified Graphics.Message.Mouse as Mouse
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

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

onClick :: (Functor m, MonadReader (RuntimeSystem r s) m) => Frame -> m (SignalT r (EnvReader r s) Mouse.Click)
onClick frame = do
  click0 <- asks (Map.lookup TClickEvent . rsEvents)
  return $ case click0 of
             Nothing -> killSignal
             Just (click, _) -> click ~~> \cev -> do
                                  env <- ask
                                  case cev of
                                    Just (ClickEvent hwnd cev')
                                        | frame == envMagicCast env hwnd -> pure cev'
                                        | otherwise -> mzero
                                    _ -> mzero
