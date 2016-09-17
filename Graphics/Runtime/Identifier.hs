{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.Runtime.Identifier(IdEnv(..), HasId(..)) where

import Control.Monad.State

class IdEnv p i | p -> i where
    makeId :: p -> (i, p)
    produceId :: MonadState p m => m i
    produceId = state makeId

class HasId a i | a -> i where
    getId :: a -> i
