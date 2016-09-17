module Graphics.Message.Unpack(MsgPack(..), defWndProc, loWord, hiWord) where

import Graphics.Windows.CCall
import Graphics.Windows.CoreTypes
import Data.Word
import Data.Int
import Data.Bits

data MsgPack = MsgPack {
      msgHwnd :: Handle,
      msgMsg :: Word32,
      msgWParam :: Word64,
      msgLParam :: Int64
    }

defWndProc :: Handle -> Word32 -> Word64 -> Int64 -> IO Int64
defWndProc = c_defWndProc

loWord :: Word64 -> Word32
loWord = fromIntegral

hiWord :: Word64 -> Word32
hiWord = fromIntegral . flip shiftR 32
