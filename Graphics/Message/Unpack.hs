module Graphics.Message.Unpack(MsgPack(..), defWndProc) where

import Graphics.Windows.CCall
import Graphics.Windows.CoreTypes
import Data.Word
import Data.Int

data MsgPack = MsgPack {
      msgHwnd :: Handle,
      msgMsg :: Word32,
      msgWParam :: Word64,
      msgLParam :: Int64
    }

defWndProc :: Handle -> Word32 -> Word64 -> Int64 -> IO Int64
defWndProc = c_defWndProc
