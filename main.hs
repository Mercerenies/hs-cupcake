
import Graphics.Reactive.System
import Graphics.Reactive.Signal
import Graphics.Windows.MessageBox
import Graphics.Windows.Control
import Graphics.Windows.Rect
import qualified Graphics.Framing.Frame as Frame
import qualified Graphics.Framing.Widget as Widget
import Graphics.Runtime
import Graphics.Runtime.Environment
import Data.Functor
import Control.Applicative

window :: Windowing r () (System r (EnvReader r ()))
window = do
  frame0 <- Frame.makeFrame
  button0 <- Widget.makeWidget Button frame0
  clicker <- onClick frame0
  let titlebar = (show <$> clicker) `defaulting` "FRP Title :)"
      buttonpos = pure $ Rect 10 10 80 30
  return (listSystem [
           pure True ~~> Frame.visibility frame0,
           titlebar ~~> Frame.titlebar frame0,
           pure "Button" ~~> Widget.text button0,
           buttonpos ~~> Widget.position button0,
           pure True ~~> Widget.visibility button0
          ])

main :: IO ()
main = doRuntime () window
