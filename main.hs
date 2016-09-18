
import Graphics.Reactive.System
import Graphics.Reactive.Signal
import Graphics.Windows.MessageBox
import Graphics.Windows.Rect
import Graphics.Types.Tagged
import qualified Graphics.Framing.Frame as Frame
import qualified Graphics.Framing.Widget as Widget
import Graphics.Runtime
import Graphics.Runtime.Environment
import Control.Applicative

window :: Windowing r () (System r (EnvReader r ()))
window = do
  frame0 <- Frame.makeFrame
  button0 <- Widget.makeButton frame0
  clicker <- Frame.onClick frame0
  buttonclick <- Widget.onClick button0
  let titlebar = (show <$> clicker <|> "Button" <$ buttonclick) `defaulting` "FRP Title :)"
      buttonpos = pure $ Rect 10 10 80 30
  return (listSystem [
           pure True ~~> Frame.visibility frame0,
           titlebar ~~> Frame.titlebar frame0,
           pure "Button" ~~> Widget.text (untag button0),
           buttonpos ~~> Widget.position (untag button0),
           pure True ~~> Widget.visibility (untag button0)
          ])

main :: IO ()
main = doRuntime () window
