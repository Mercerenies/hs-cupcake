{-# LANGUAGE DataKinds, MultiParamTypeClasses #-}

module Graphics.Message.Decode(MsgEvent(..), EventType(..), decodeMsg) where

import Graphics.Windows.CoreTypes
import Graphics.Windows.Control
import Graphics.Types.Tagged
import Graphics.Types.Pointer
import Graphics.Message.Unpack
import qualified Graphics.Message.Mouse as Mouse
import Control.Applicative

data MsgEvent = ClickEvent Handle Mouse.Click |
                ButtonEvent (Tagged 'Button Control)
                deriving (Show, Eq)

data EventType = TClickEvent | TButtonEvent
                 deriving (Show, Read, Eq, Ord, Enum)

instance TagType 'TClickEvent EventType where
    toTEnum _ = TClickEvent

instance TagType 'TButtonEvent EventType where
    toTEnum _ = TButtonEvent

instance Taggable EventType MsgEvent where
    getTag (ClickEvent {}) = TClickEvent
    getTag (ButtonEvent {}) = TButtonEvent

decodeMouse :: MsgPack -> Maybe MsgEvent
decodeMouse (MsgPack hwnd msg _ _) = case msg of
                                       0x0201 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Left Mouse.Down)
                                       0x0202 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Left Mouse.Up)
                                       0x0207 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Middle Mouse.Down)
                                       0x0208 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Middle Mouse.Up)
                                       0x0204 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Right Mouse.Down)
                                       0x0205 -> Just $ ClickEvent hwnd (Mouse.Click Mouse.Right Mouse.Up)
                                       _ -> Nothing

decodeCommand :: MsgPack -> Maybe MsgEvent
decodeCommand (MsgPack _ 0x0111  _  0) = Nothing -- TODO The other uses for decodeCommand
decodeCommand (MsgPack _ 0x0111 wp lp) = case hiWord wp of
                                           0x0000 -> do
                                             ptr <- intToPtr lp
                                             ptr' <- tag . toControl Button . Handle $ ptr
                                             return $ ButtonEvent ptr'
                                           _ -> Nothing
decodeCommand _ = Nothing

decodeMsg :: MsgPack -> Maybe MsgEvent
decodeMsg pack = decodeMouse pack <|> decodeCommand pack
