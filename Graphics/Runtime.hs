{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Graphics.Runtime(RuntimeSystem, Windowing(Windowing), makeFrame, bindTitleBar, bindVisibility,
                        onClick, doRuntime, EnvReader, RuntimeEnv, RuntimeSetup) where

import Graphics.Windows.CCall(c_newWndProc, c_setWndPtr)
import Graphics.Reactive.Signal
import Graphics.Reactive.System
import Graphics.Windows.Window as Window
import Graphics.Windows.CoreTypes
import qualified Graphics.Message.Mouse as Mouse
import Graphics.Message.Unpack
import Graphics.Message.Decode
import Data.Functor
import Data.Word
import Data.Int
import Data.IORef
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Foreign.Ptr

data RuntimeSystem r s = RuntimeSystem {
      rsBindVisibility :: Frame -> SignalT r (EnvReader r s) Bool -> System r (EnvReader r s),
      rsBindTitleBar :: Frame -> SignalT r (EnvReader r s) String -> System r (EnvReader r s),
      rsRun :: System r (EnvReader r s) -> EnvReader r s (),
      rsClick :: SignalT r (EnvReader r s) (Maybe (Handle, Mouse.Click))
    }

data RuntimeSetup r s = RuntimeSetup {
      rspId :: Integer,
      rspFrames :: [Frame]
    }

data RuntimeEnv r = RuntimeEnv {
      frameToHandle :: Frame -> Handle,
      handleToFrame :: Handle -> Frame
    }

type EnvReader r s = MaybeT (RWST (RuntimeEnv r) () s IO)

newtype Frame = Frame { fUnique :: Integer }
    deriving (Eq, Ord)

-- TODO Transformer?
newtype Windowing r s a = Windowing { unWindowing :: RWS (RuntimeSystem r s) () (RuntimeSetup r s) a }
    deriving (Functor, Applicative, Monad)

instance MonadReader (RuntimeSystem r s) (Windowing r s) where
    ask = Windowing ask
    local ff (Windowing m) = Windowing $ local ff m

instance MonadState (RuntimeSetup r s) (Windowing r s) where
    get = Windowing get
    put = Windowing . put

instance MonadWriter () (Windowing r s) where
    tell = Windowing . tell
    listen (Windowing x) = Windowing $ listen x
    pass (Windowing x) = Windowing $ pass x

runEnvReader :: EnvReader r s a -> RuntimeEnv r -> s -> IO (Maybe a, s, ())
runEnvReader env = runRWST (runMaybeT env)

rspUpdateId :: Windowing r s Integer
rspUpdateId = do
  rsp <- get
  put $ rsp { rspId = rspId rsp + 1 }
  return $ rspId rsp

makeFrame :: Windowing r s Frame
makeFrame = do
  frame <- Frame <$> rspUpdateId
  rsp <- get
  put $ rsp { rspFrames = frame : rspFrames rsp }
  return frame

runtimeSetup :: RuntimeSetup r s
runtimeSetup = RuntimeSetup {
                   rspId = 0,
                   rspFrames = []
                 }

setupEnv :: RuntimeSetup r s -> IO (RuntimeEnv r)
setupEnv rsp = do
  map0 <- mapM (const $ makeWindow "") (rspFrames rsp)
  let map1 = Map.fromList $ zip (rspFrames rsp) map0
      map1' = Map.fromList $ zip map0 (rspFrames rsp)
      lookupFunc x = Map.findWithDefault (error "invalid frame state") x map1 -- TODO Handle error
      lookupFunc' x = Map.findWithDefault (error "invalid frame state") x map1' -- TODO Handle error
  return $ RuntimeEnv {
                 frameToHandle = lookupFunc,
                 handleToFrame = lookupFunc'
               }

runWindowing :: Windowing r s a -> RuntimeSystem r s -> (a, RuntimeSetup r s)
runWindowing (Windowing rws0) rs = (\(a, r, _) -> (a, r)) $ runRWS rws0 rs runtimeSetup

bindTitleBar :: Frame -> SignalT r (EnvReader r s) String -> Windowing r s (System r (EnvReader r s))
bindTitleBar hwnd sign = reader $ \rs -> rsBindTitleBar rs hwnd sign

bindVisibility :: Frame -> SignalT r (EnvReader r s) Bool -> Windowing r s (System r (EnvReader r s))
bindVisibility hwnd sign = reader $ \rs -> rsBindVisibility rs hwnd sign

onClick :: Frame -> Windowing r s (SignalT r (EnvReader r s) (Maybe Mouse.Click))
onClick frame = do
  click <- asks rsClick
  return $ click ~~> \cev -> do
                  env <- ask
                  return $ case cev of
                             Nothing -> Nothing
                             Just (hwnd, cev')
                                 | handleToFrame env hwnd == frame -> Just cev'
                                 | otherwise -> Nothing

initRuntime :: IO (RuntimeSystem r s)
initRuntime = do
  (clickTest0, clickTestFire) <- newSignalT Nothing
  let titlebar frame sign = singletonSystem $ sign ~~> \str -> do
                              hwnd <- asks $ flip frameToHandle frame
                              liftIO $ setWindowTitle hwnd str
      visibility frame sign = singletonSystem $ sign ~~> \bool -> do
                                                                 hwnd <- asks $ flip frameToHandle frame
                                                                 if bool then
                                                                     liftIO $ showWindow hwnd
                                                                 else
                                                                     liftIO $ hideWindow hwnd
      processMsg sys0 hwnd msg wparam lparam = do
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
                fireInitEvent sysInit
                liftIO $ writeIORef sys sysInit
                liftIO $ messageLoop
                liftIO $ c_setWndPtr nullFunPtr
                liftIO $ freeHaskellFunPtr procPtr
      rs = RuntimeSystem {
                rsBindTitleBar = titlebar,
                rsBindVisibility = visibility,
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
