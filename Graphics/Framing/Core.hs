{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Framing.Core(Frame, newFrame, Widget, newWidget, widgetOwner) where

import Graphics.Windows.Control
import Graphics.Runtime.Identifier
import Graphics.Types.Tagged

newtype Frame = Frame { fUnique :: Integer }
    deriving (Eq, Ord)

instance HasId Frame Integer where
    getId = fUnique

newFrame :: Integer -> Frame
newFrame = Frame

data Widget = Widget { wType :: CtrlType, wParent :: Frame, wUnique :: Integer }
              deriving (Eq, Ord)

instance HasId Widget Integer where
    getId = wUnique

instance Taggable CtrlType Widget where
    getTag = wType

newWidget :: CtrlType -> Frame -> Integer -> Widget
newWidget = Widget

widgetOwner :: Widget -> Frame
widgetOwner = wParent
