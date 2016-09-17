module Graphics.Windows.CoreTypes(Handle(..), PtrWrapper(..)) where

import Foreign.Ptr

newtype Handle = Handle (Ptr ())
    deriving (Eq, Ord)

class PtrWrapper a where
    getPtr :: a -> Ptr ()
    putPtr :: Ptr () -> a

instance PtrWrapper Handle where
    getPtr (Handle x) = x
    putPtr = Handle
