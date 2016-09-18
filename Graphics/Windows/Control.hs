{-# LANGUAGE MultiParamTypeClasses, DataKinds #-}

module Graphics.Windows.Control(Control, CtrlType(..), getCtrlText, setCtrlText,
                                ctrlHwnd, makeButton, makeCtrl,
                                setCtrlPos, showCtrl, hideCtrl, toControl) where

import Graphics.Windows.CCall
import Graphics.Windows.CoreTypes
import Graphics.Windows.Window
import Graphics.Windows.Rect
import Graphics.Types.Tagged
import Foreign.C.String
import Foreign.Marshal.Alloc

data Control = Control CtrlType Handle
               deriving (Eq, Ord, Show)

data CtrlType = Button
                deriving (Show, Read, Eq, Ord, Enum)

makeCtrl' :: String -> String -> Handle -> IO Handle
makeCtrl' cls str par = do
  cCls <- newCString cls
  cStr <- newCString str
  hwnd <- c_makeCtrl cCls cStr par
  free cStr
  free cCls
  return hwnd

getCtrlText :: Control -> IO String
getCtrlText (Control _ hwnd) = getWindowTitle hwnd

setCtrlText :: Control -> String -> IO ()
setCtrlText (Control _ hwnd) = setWindowTitle hwnd

setCtrlPos :: Control -> Rect -> IO ()
setCtrlPos (Control _ hwnd) = setWindowPos hwnd

ctrlHwnd :: Control -> Handle
ctrlHwnd (Control _ hwnd) = hwnd

showCtrl :: Control -> IO ()
showCtrl (Control _ hwnd) = showWindow hwnd

hideCtrl :: Control -> IO ()
hideCtrl (Control _ hwnd) = hideWindow hwnd

makeButton :: String -> Handle -> IO Control
makeButton text par = do
  hwnd <- makeCtrl' "BUTTON" text par
  return $ Control Button hwnd

makeCtrl :: String -> CtrlType -> Handle -> IO Control
makeCtrl str ctype hwnd = Control ctype <$> makeCtrl' (ctrlClsName ctype) str hwnd

ctrlClsName :: CtrlType -> String
ctrlClsName Button = "BUTTON"

toControl :: CtrlType -> Handle -> Control
toControl = Control

instance TagType 'Button CtrlType where
    toTEnum _ = Button

instance Taggable CtrlType Control where
    getTag (Control x _) = x
