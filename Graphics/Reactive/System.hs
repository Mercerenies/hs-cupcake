module Graphics.Reactive.System(System, fireEvent, fireInitEvent,
                                newSystem, singletonSystem, listSystem, newSignalT, newSignalT_,
                                (<&&)) where

import Graphics.Reactive.Event
import Graphics.Reactive.Signal
import Data.Map(Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

infixl 6 <&&

data System s m = System (Map (Event s) Integer) [SignalT s m ()]

instance Monoid (System s m) where
    mempty = System Map.empty []
    System m0 xs `mappend` System m1 ys = System (Map.unionWith (+) m0 m1) $ xs `mappend` ys
    mconcat ss = System (foldl (Map.unionWith (+)) Map.empty $ map (\(System mm _) -> mm) ss) (concatMap (\(System _ xs) -> xs) ss)

when_ :: Applicative m => Bool -> m () -> m ()
when_ True = id
when_ False = const $ pure ()

fireEvent :: Alternative m => System s m -> Event s -> m (System s m)
fireEvent (System mm xs) InitEvent = let fireOnce x = void (runSignalT x InitEvent mm) <|> pure ()
                                     in System mm xs <$ traverse fireOnce xs
fireEvent (System mm xs) ev = let newMap = Map.alter (liftA2 (<|>) (fmap (+1)) (const $ Just 1)) ev mm
                                  fireOnce x = when_ (x `dependsOn` ev) $
                                               void (runSignalT x ev newMap) <|> pure ()
                              in System newMap xs <$ traverse fireOnce xs

fireInitEvent :: Alternative m => System s m -> m (System s m)
fireInitEvent = flip fireEvent InitEvent

newSystem :: System s m
newSystem = mempty

singletonSystem :: SignalT s m () -> System s m
singletonSystem s = System Map.empty [s]

listSystem :: [SignalT s m ()] -> System s m
listSystem = System Map.empty

newSignalT :: (Alternative m, MonadIO m) => a -> IO (SignalT s m a, System s m -> a -> m (System s m))
newSignalT init0 = do
  ref <- newIORef init0
  event <- newEvent
  let reader = triggerOn event *> react (liftIO $ readIORef ref)
      writer system x = do
        liftIO $ writeIORef ref x
        fireEvent system event
  return (reader, writer)

newSignalT_ :: (Alternative m, MonadIO m) => IO (SignalT s m (), System s m -> m (System s m))
newSignalT_ = do
  (sign, f) <- newSignalT ()
  return (sign, \s -> f s ())

(<&&) :: System s m -> SignalT s m () -> System s m
System ev xs <&& x = System ev (x : xs)
