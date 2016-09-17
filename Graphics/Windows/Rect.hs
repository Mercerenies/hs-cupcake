module Graphics.Windows.Rect(Rect(..)) where

data Rect = Rect {
      getX :: Int,
      getY :: Int,
      getW :: Int,
      getH :: Int
    } deriving (Show, Read, Eq)
