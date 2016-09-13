{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Windows.CCall where

import Foreign.C.String
import Graphics.Windows.CoreTypes
import Data.Int
import Data.Word
import Foreign.Ptr

foreign import ccall "messageBox" c_messageBox :: CString -> CString -> Int -> IO Int
foreign import ccall "makeWindow" c_makeWindow :: CString -> IO Handle
foreign import ccall "showWindow" c_showWindow :: Handle -> IO ()
foreign import ccall "hideWindow" c_hideWindow :: Handle -> IO ()
foreign import ccall "messageLoop" c_messageLoop :: IO ()
foreign import ccall "setWindowTitle" c_setWindowTitle :: Handle -> CString -> IO ()
foreign import ccall "getWindowTitle" c_getWindowTitle :: Handle -> IO CString
foreign import ccall "defWndProc" c_defWndProc :: Handle -> Word32 -> Word64 -> Int64 -> IO Int64
foreign import ccall "setWndPtr" c_setWndPtr :: FunPtr (Handle -> Word32 -> Word64 -> Int64 -> IO Int64) -> IO ()

foreign import ccall "wrapper" c_newWndProc :: (Handle -> Word32 -> Word64 -> Int64 -> IO Int64) ->
                                               IO (FunPtr (Handle -> Word32 -> Word64 -> Int64 -> IO Int64))
