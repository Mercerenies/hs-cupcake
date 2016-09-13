module Graphics.Reactive.System(System, fireEvent, fireInitEvent,
                                newSystem, singletonSystem, newSignalT, newSignalT_,
                                (<&>)) where

import Graphics.Reactive.Event
import Graphics.Reactive.Signal
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.IORef
import Data.Foldable hiding (concatMap, foldl)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

data System s m = System (Map (Event s) Integer) [SignalT s m ()]

instance Monoid (System s m) where
    mempty = System Map.empty []
    System m0 xs `mappend` System m1 ys = System (Map.unionWith (+) m0 m1) $ xs `mappend` ys
    mconcat ss = System (foldl (Map.unionWith (+)) Map.empty $ map (\(System mm _) -> mm) ss) (concatMap (\(System _ xs) -> xs) ss)

when_ :: Applicative m => Bool -> m () -> m ()
when_ True = id
when_ False = const $ pure ()

fireEvent :: Applicative m => System s m -> Event s -> m (System s m)
fireEvent (System mm xs) InitEvent = System mm xs <$ (for_ xs $ \x -> runSignalT x InitEvent mm)
fireEvent (System mm xs) ev = let newMap = Map.alter (liftA2 (<|>) (fmap (+1)) (const $ Just 1)) ev mm
                              in System newMap xs <$
                                     (for_ xs $ \x -> when_ (x `dependsOn` ev) $ void (runSignalT x ev newMap))

fireInitEvent :: Applicative m => System s m -> m (System s m)
fireInitEvent = flip fireEvent InitEvent

newSystem :: System s m
newSystem = mempty

singletonSystem :: SignalT s m () -> System s m
singletonSystem s = System Map.empty [s]

newSignalT :: (Applicative m, MonadIO m) => a -> IO (SignalT s m a, System s m -> a -> m (System s m))
newSignalT init0 = do
  ref <- newIORef init0
  event <- newEvent
  let reader = triggerOn event *> react (liftIO $ readIORef ref)
      writer system x = do
        liftIO $ writeIORef ref x
        fireEvent system event
  return (reader, writer)

newSignalT_ :: (Applicative m, MonadIO m) => IO (SignalT s m (), System s m -> m (System s m))
newSignalT_ = do
  (sign, f) <- newSignalT ()
  return (sign, \s -> f s ())

(<&>) :: Applicative m => m (System s m0) -> m (System s m0) -> m (System s m0)
(<&>) = liftA2 (<>)
