{-# LANGUAGE PolyKinds, MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.Types.Tagged where

newtype Tagged t a = Tagged a
    deriving (Eq, Ord)

class Eq v => TagType t v | t -> v where
    toTEnum :: proxy t -> v
    tagEq :: proxy t -> v -> Bool
    val `tagEq` ctp = toTEnum val == ctp

class Taggable v a where
    getTag :: a -> v

untag :: Tagged t a -> a
untag (Tagged a) = a

tag :: (TagType t v, Taggable v a) => a -> Maybe (Tagged t a)
tag a = let result = if toP result `tagEq` getTag a then
                         Just $ Tagged a
                     else
                         Nothing
            toP :: Maybe (Tagged t' a') -> p t'
            toP = undefined
        in result
