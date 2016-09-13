
import Graphics.Util
import Graphics.Reactive.System
import Graphics.Reactive.Signal
import Graphics.Windows.CoreTypes
import Graphics.Windows.MessageBox
import Graphics.Windows.Window
import Graphics.Runtime
import Data.Monoid
import Data.Functor
import Control.Applicative

{-
main :: IO ()
main = do
  rs <- initRuntime
  (counter, setCounter) <- newSignalT 0
  (titlebar, setTitlebar) <- newSignalT ""
  countStepper <- stepValue 0 (+ 1)
  let titlebar0 = rsInit rs *> react (setTitlebar system "FRP Title :)")
      clickEvent = rsClickTest rs *> (countStepper ~~> setCounter system)
      titlebar1 = counter ~~> (setTitlebar system . show)
      system = rsBindTitleBar rs titlebar <>
               singletonSystem titlebar0 <>
               singletonSystem clickEvent <>
               singletonSystem titlebar1
  rsRun rs system

main :: IO ()
main = do
  rs <- initRuntime
  counter <- stepValue 0 (+ 1)
  let titlebar0 = rsInit rs $> "FRP Title :)"
      clickEvent = rsClickTest rs @> counter
      titlebar1 = maybe "" id <$> ((fmap show <$> clickEvent) <--> hairpin titlebar0)
      system = rsBindTitleBar rs titlebar1
  rsRun rs system

main :: IO ()
main = do
  rs <- initRuntime
  let titlebar1 = ((show <$> stepperOn (rsClickTest rs)) <~~> (rsInit rs $> "FRP Title :)")) `defaulting` ""
  rsRun rs (rsBindTitleBar rs titlebar1)

-}

-- ///// Test with nontrivial state (the parameter after r)

window :: Windowing r () (System r (EnvReader r ()))
window = do
  frame0 <- makeFrame
  clicker <- onClick frame0
  let titlebar = (fmap show <$> clicker) `defaulting` "FRP Title :)"
  bindTitleBar frame0 titlebar <&> bindVisibility frame0 (pure True)

main :: IO ()
main = doRuntime () window
