{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.Reactive.EmbedRef where

import Data.STRef
import Data.IORef
import Control.Monad.ST
import Control.Applicative

class (Applicative m, Monad m) => EmbedRef r m | r -> m, m -> r where
    newRef :: a -> m (r a)
    readRef :: r a -> m a
    writeRef :: r a -> a -> m ()

    modifyRef :: r a -> (a -> a) -> m ()
    modifyRef val f = readRef val >>= writeRef val . f

instance EmbedRef IORef IO where
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef
    modifyRef = modifyIORef

instance EmbedRef (STRef s) (ST s) where
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef
    modifyRef = modifySTRef
