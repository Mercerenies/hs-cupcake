module Graphics.Message.Decode(MsgEvent(..), decodeMsg) where

import Graphics.Message.Unpack
import qualified Graphics.Message.Mouse as Mouse

data MsgEvent = ClickEvent Mouse.Click

decodeMouse :: MsgPack -> Maybe MsgEvent
decodeMouse (MsgPack _ msg _ _) = case msg of
                                    0x0201 -> Just $ ClickEvent (Mouse.Click Mouse.Left Mouse.Down)
                                    0x0202 -> Just $ ClickEvent (Mouse.Click Mouse.Left Mouse.Up)
                                    0x0207 -> Just $ ClickEvent (Mouse.Click Mouse.Middle Mouse.Down)
                                    0x0208 -> Just $ ClickEvent (Mouse.Click Mouse.Middle Mouse.Up)
                                    0x0204 -> Just $ ClickEvent (Mouse.Click Mouse.Right Mouse.Down)
                                    0x0205 -> Just $ ClickEvent (Mouse.Click Mouse.Right Mouse.Up)
                                    _ -> Nothing

decodeMsg :: MsgPack -> Maybe MsgEvent
decodeMsg pack = decodeMouse pack
