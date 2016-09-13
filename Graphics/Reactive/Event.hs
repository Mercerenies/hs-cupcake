module Graphics.Reactive.Event(Event(InitEvent), newEvent) where

import Data.Unique
import Data.Functor

data Event s = InitEvent | Event Unique
               deriving (Eq, Ord)

newEvent :: IO (Event s)
newEvent = Event <$> newUnique
