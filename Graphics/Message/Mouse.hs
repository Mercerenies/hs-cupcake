module Graphics.Message.Mouse where

data Button = Left | Middle | Right
              deriving (Show, Read, Eq, Ord, Enum)

data State = Up | Down
             deriving (Show, Read, Eq, Ord, Enum)

data Click = Click Button State
             deriving (Show, Read, Eq)

getButton :: Click -> Button
getButton (Click b _) = b

getState :: Click -> State
getState (Click _ s) = s
