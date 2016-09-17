{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Runtime(Windowing, doRuntime, EnvReader) where

import Graphics.Windows.CCall(c_newWndProc, c_setWndPtr)
import Graphics.Reactive.System
import Graphics.Windows.Window as Window
import Graphics.Windows.CoreTypes
import Graphics.Runtime.Environment
import Graphics.Runtime.Framework
import Graphics.Message.Unpack
import Graphics.Message.Decode
import Graphics.Types.Tagged
import Data.Functor
import Data.Word
import Data.Int
import Data.IORef
import qualified Data.Map as Map
import Control.Monad.RWS
import Foreign.Ptr

initRuntime :: forall r s. IO (RuntimeSystem r s)
initRuntime = do
  clicker <- newSignalT Nothing
  let processMsg :: System r (EnvReader r s) -> Handle -> Word32 -> Word64 -> Int64 ->
                    EnvReader r s (Int64, System r (EnvReader r s))
      processMsg sys0 hwnd msg wparam lparam = do
                let pack = MsgPack hwnd msg wparam lparam
                    msgEv = decodeMsg pack
                    def = (\x -> (x, sys0)) <$> (lift . lift $ defWndProc hwnd msg wparam lparam)
                case msgEv of
                  Nothing -> def
                  Just ev -> case Map.lookup (getTag ev) $ rsEvents rs of
                               Nothing -> def
                               Just (_, fire) -> (,) 0 <$> fire sys0 (Just ev) -- TODO Nonzero return in some cases
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
                rsEvents = Map.fromList [(TClickEvent, clicker)]
              }
  return rs

doRuntime :: s -> Windowing r s (System r (EnvReader r s)) -> IO ()
doRuntime s rdr = do
  rs <- initRuntime
  let (sys, setup) = runWindowing rdr rs
  env <- setupEnv setup
  void $ runEnvReader (rsRun rs sys) env s
