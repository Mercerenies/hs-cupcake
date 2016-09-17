{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Graphics.Runtime.Framework(RuntimeSystem(..), Windowing, runWindowing) where

import Graphics.Runtime.Environment
import Graphics.Reactive.Signal
import Graphics.Reactive.System
import Graphics.Windows.CoreTypes
import qualified Graphics.Message.Mouse as Mouse
import Control.Applicative
import Control.Monad.RWS

data RuntimeSystem r s = RuntimeSystem {
      rsRun :: System r (EnvReader r s) -> EnvReader r s (),
      rsClick :: SignalT r (EnvReader r s) (Maybe (Handle, Mouse.Click))
    }

-- TODO Transformer?
newtype Windowing r s a = Windowing (RWS (RuntimeSystem r s) () (RuntimeSetup r s) a)
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

runWindowing :: Windowing r s a -> RuntimeSystem r s -> (a, RuntimeSetup r s)
runWindowing (Windowing rws0) rs = (\(a, r, _) -> (a, r)) $ runRWS rws0 rs runtimeSetup
