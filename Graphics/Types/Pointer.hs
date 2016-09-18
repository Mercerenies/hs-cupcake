module Graphics.Types.Pointer(intToPtr, wordToPtr) where

import Data.Ix
import Data.Function
import Control.Arrow
import Foreign.Ptr

toPtrIntermediate :: (Integral a, Integral a0, Bounded a0) => (a0 -> Ptr b) -> a -> Maybe (Ptr b)
toPtrIntermediate ff x = let tagVal = fix (undefined . ff)
                             bounds = (minBound, maxBound) `asTypeOf` (tagVal, tagVal)
                             bounds' = (toInteger *** toInteger) bounds
                         in if bounds' `inRange` toInteger x then
                                Just . ff $ fromIntegral x
                            else
                                Nothing
intToPtr :: Integral a => a -> Maybe (Ptr b)
intToPtr = toPtrIntermediate intPtrToPtr

wordToPtr :: Integral a => a -> Maybe (Ptr b)
wordToPtr = toPtrIntermediate wordPtrToPtr
