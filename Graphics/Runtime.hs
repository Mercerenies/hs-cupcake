module Graphics.Runtime(Windowing, onClick, doRuntime, EnvReader) where

import Graphics.Windows.CCall(c_newWndProc, c_setWndPtr)
import Graphics.Reactive.Signal
import Graphics.Reactive.System
import Graphics.Windows.Window as Window
import Graphics.Windows.CoreTypes
import Graphics.Runtime.Environment
import Graphics.Runtime.Framework
import qualified Graphics.Message.Mouse as Mouse
import Graphics.Message.Unpack
import Graphics.Message.Decode
import Data.Functor
import Data.Word
import Data.Int
import Data.IORef
import Control.Applicative
import Control.Monad.RWS
import Foreign.Ptr

onClick :: Frame -> Windowing r s (SignalT r (EnvReader r s) Mouse.Click)
onClick frame = do
  click <- asks rsClick
  return $ click ~~> \cev -> do
                  env <- ask
                  case cev of
                    Nothing -> mzero
                    Just (hwnd, cev')
                        | frame == envMagicCast env hwnd -> pure cev'
                        | otherwise -> mzero

initRuntime :: IO (RuntimeSystem r s)
initRuntime = do
  (clickTest0, clickTestFire) <- newSignalT Nothing
  let processMsg sys0 hwnd msg wparam lparam = do
                let pack = MsgPack hwnd msg wparam lparam
                    msgEv = decodeMsg pack
                case msgEv of
                  Just (ClickEvent click) -> ((,) 0) <$> (clickTestFire sys0 $ Just (hwnd, click))
                  Nothing -> (\x -> (x, sys0)) <$> (lift . lift $ defWndProc hwnd msg wparam lparam)
      runner system = do
                st <- get
                sys <- liftIO $ newIORef system
                stat <- liftIO $ newIORef st
                env <- ask
                let wndProc :: Handle -> Word32 -> Word64 -> Int64 -> IO Int64
                    wndProc hwnd' msg wparam lparam = do
                        st0 <- readIORef stat
                        sys0 <- readIORef sys
                        (mb, st1, _) <- runEnvReader (processMsg sys0 hwnd' msg wparam lparam) env st0
                        res' <- case mb of
                                  Just (res, sys1) -> writeIORef sys sys1 >> return res
                                  Nothing -> return 0
                        writeIORef stat st1
                        return res'
                procPtr <- liftIO $ c_newWndProc wndProc
                liftIO $ c_setWndPtr procPtr
                sysInit <- liftIO $ readIORef sys
                sysInit' <- fireInitEvent sysInit
                liftIO $ writeIORef sys sysInit'
                liftIO $ messageLoop
                liftIO $ c_setWndPtr nullFunPtr
                liftIO $ freeHaskellFunPtr procPtr
      rs = RuntimeSystem {
                rsRun = runner,
                rsClick = clickTest0
              }
  return rs

doRuntime :: s -> Windowing r s (System r (EnvReader r s)) -> IO ()
doRuntime s rdr = do
  rs <- initRuntime
  let (sys, setup) = runWindowing rdr rs
  env <- setupEnv setup
  void $ runEnvReader (rsRun rs sys) env s
