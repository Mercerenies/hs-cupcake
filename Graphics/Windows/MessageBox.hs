module Graphics.Windows.MessageBox(MBoxButton(..), MBoxIcon(..), MBoxResult(..),
                                   messageBox) where

import Graphics.Windows.CCall
import Foreign.C.String
import Foreign.Marshal.Alloc
import Data.Bits

data MBoxButton = ButtonAbReIg | ButtonCaTrCo | ButtonHelp | ButtonOk | ButtonOkCancel |
                  ButtonRetryCancel | ButtonYesNo | ButtonYesNoCancel
                  deriving (Eq, Ord, Enum, Show, Read)

data MBoxIcon = IconNull | IconExcl | IconInfo | IconQuest | IconErr
                deriving (Eq, Ord, Enum, Show, Read)

data MBoxResult = ResultAbort | ResultCancel | ResultContinue | ResultIgnore | ResultNo | ResultOk |
                  ResultRetry | ResultTryAgain | ResultYes | ResultFailure
                  deriving (Eq, Ord, Enum, Show, Read)

mboxButtonValue :: MBoxButton -> Int
mboxButtonValue ButtonAbReIg = 0x2
mboxButtonValue ButtonCaTrCo = 0x6
mboxButtonValue ButtonHelp = 0x4000
mboxButtonValue ButtonOk = 0x0
mboxButtonValue ButtonOkCancel = 0x1
mboxButtonValue ButtonRetryCancel = 0x5
mboxButtonValue ButtonYesNo = 0x4
mboxButtonValue ButtonYesNoCancel = 0x3

mboxIconValue :: MBoxIcon -> Int
mboxIconValue IconExcl = 0x30
mboxIconValue IconInfo = 0x40
mboxIconValue IconQuest = 0x20
mboxIconValue IconErr = 0x10
mboxIconValue IconNull = 0x00

mboxResult :: Int -> MBoxResult
mboxResult 3 = ResultAbort
mboxResult 2 = ResultCancel
mboxResult 11 = ResultContinue
mboxResult 5 = ResultIgnore
mboxResult 7 = ResultNo
mboxResult 1 = ResultOk
mboxResult 4 = ResultRetry
mboxResult 10 = ResultTryAgain
mboxResult 6 = ResultYes
mboxResult _ = ResultFailure

messageBox :: String -> String -> MBoxIcon -> MBoxButton -> IO MBoxResult
messageBox text caption icn btn = do
  cText <- newCString text
  cCaption <- newCString caption
  let type_ = mboxButtonValue btn .|. mboxIconValue icn
  result <- c_messageBox cText cCaption type_
  free cText
  free cCaption
  return $ mboxResult result
