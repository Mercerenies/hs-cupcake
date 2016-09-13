module Graphics.Windows.Window(makeWindow, showWindow, hideWindow, messageLoop,
                               getWindowTitle, setWindowTitle) where

import Graphics.Windows.CCall
import Graphics.Windows.CoreTypes
import Foreign.C.String
import Foreign.Marshal.Alloc

makeWindow :: String -> IO Handle
makeWindow str = do
  cStr <- newCString str
  hwnd <- c_makeWindow cStr
  free cStr
  return hwnd

showWindow :: Handle -> IO ()
showWindow = c_showWindow

hideWindow :: Handle -> IO ()
hideWindow = c_hideWindow

messageLoop :: IO ()
messageLoop = c_messageLoop

getWindowTitle :: Handle -> IO String
getWindowTitle hwnd = do
  cStr <- c_getWindowTitle hwnd
  str <- peekCString cStr
  free cStr
  return str

setWindowTitle :: Handle -> String -> IO ()
setWindowTitle hwnd str = do
  cStr <- newCString str
  c_setWindowTitle hwnd cStr
  free cStr
