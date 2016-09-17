{-# LANGUAGE DataKinds, MultiParamTypeClasses #-}

module Graphics.Message.Decode(MsgEvent(..), EventType(..), decodeMsg) where

import Graphics.Windows.CoreTypes
import Graphics.Types.Tagged
import Graphics.Message.Unpack
import qualified Graphics.Message.Mouse as Mouse

data MsgEvent = ClickEvent Handle Mouse.Click |
                ButtonEvent (Tagged Button Control)

data EventType = TClickEvent | TButtonEvent
                 deriving (Show, Read, Eq, Ord, Enum)

instance TagType TClickEvent EventType where
    toTEnum _ = TClickEvent

instance TagType TButtonEvent EventType where
    toTEnum _ = TButtonEvent

instance Taggable EventType MsgEvent where
    getTag (ClickEvent {}) = TClickEvent

decodeMouse :: MsgPack -> Maybe MsgEvent
decodeMouse (MsgPack hwnd msg _ _) = case msg of
                                       0x0201 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Left Mouse.Down)
                                       0x0202 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Left Mouse.Up)
                                       0x0207 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Middle Mouse.Down)
                                       0x0208 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Middle Mouse.Up)
                                       0x0204 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Right Mouse.Down)
                                       0x0205 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Right Mouse.Up)
                                       _ -> Nothing
{-
decodeCommand :: MsgPack -> Maybe MsgEvent
decodeCommand (MsgPack _ msg wp lp) = toControl
-} -- ///// How to get WPARAM and LPARAM to ptrs? Maybe by interpreting them as IntPtr / WordPtr
decodeMsg :: MsgPack -> Maybe MsgEvent
decodeMsg pack = decodeMouse pack
