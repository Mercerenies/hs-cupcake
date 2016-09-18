module Graphics.Reactive.Signal(Event, SignalT, Signal, react, triggerOn, signalT, dependsOn,
                                runSignalT, runSignal, liftSignal, (~~>), killSignal,
                                hairpin, (<-->), initially, (<~~>), (@>), (@->), stepper, stepperOn) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Functor.Identity
import Data.Foldable as Fold
import Data.Function
import Control.Applicative
import Control.Monad
import Graphics.Reactive.Event
import Graphics.Util

data SignalT s m a = SignalT (Set (Event s)) (Event s -> (Map (Event s) Integer) -> m a)

type Signal s = SignalT s Identity

instance Functor m => Functor (SignalT s m) where
    fmap f (SignalT ev x) = SignalT ev $ \k v -> f <$> x k v

instance Applicative m => Applicative (SignalT s m) where
    pure = SignalT Set.empty . pure . pure . pure
    SignalT ev0 ff <*> SignalT ev1 xx = SignalT (ev0 `Set.union` ev1) $ \k ev -> ff k ev <*> xx k ev

instance Alternative m => Alternative (SignalT s m) where
    empty = react empty
    SignalT ev0 xx <|> SignalT ev1 yy = SignalT (ev0 `Set.union` ev1) $ \k ev -> xx k ev <|> yy k ev

react :: m a -> SignalT s m a
react x = SignalT Set.empty $ \_ _ -> x

triggerOn :: Applicative m => Event s -> SignalT s m ()
triggerOn ev = SignalT (Set.singleton ev) $ pure (pure (pure ()))

signalT :: (Event s -> Map (Event s) Integer -> m a) -> SignalT s m a
signalT = SignalT Set.empty

dependsOn :: SignalT s m a -> Event s -> Bool
dependsOn (SignalT ev0 _) ev = ev `Set.member` ev0

runSignalT :: SignalT s m a -> Event s -> Map (Event s) Integer -> m a
runSignalT (SignalT _ x) = x

runSignal :: Signal s a -> Event s -> Map (Event s) Integer -> a
runSignal = (fmap runIdentity .) . runSignalT

liftSignal :: Applicative m => Signal s a -> SignalT s m a
liftSignal (SignalT ev x) = SignalT ev $ \k ev' -> pure (runIdentity $ x k ev')

(~~>) :: Monad m => SignalT s m a -> (a -> m b) -> SignalT s m b
SignalT ev0 xx ~~> ff = SignalT ev0 $ \k ev -> xx k ev >>= ff

{-
stepValue :: EmbedRef r m => a -> (a -> a) -> m (SignalT s m a)
stepValue init0 f = do
  ref <- newRef init0
  return $ react (modifyRef ref f *> readRef ref)
-}

killSignal :: Alternative m => SignalT s m a
killSignal = empty

hairpin :: Alternative m => SignalT s m a -> SignalT s m a
hairpin (SignalT ev0 x) = SignalT ev0 $ \ev k -> guardA (ev `Set.member` ev0) *> x ev k

(<-->) :: (Applicative m, Alternative f) => SignalT s m (f a) -> SignalT s m (f a) -> SignalT s m (f a)
sign0 <--> sign1 = liftA2 (<|>) sign0 sign1

initially :: (Alternative m) => SignalT s m a -> a -> SignalT s m a
sign0 `initially` x = sign0 <|> triggerOn InitEvent @> pure x

(<~~>) :: (Alternative m) => SignalT s m a -> SignalT s m a -> SignalT s m a
(<~~>) = (<|>) `on` hairpin

(@>) :: Alternative m => SignalT s m a -> SignalT s m b -> SignalT s m b
SignalT ev0 _ @> SignalT ev1 y = SignalT (ev0 `Set.union` ev1) $ \ev k -> if ev `Set.member` ev0 then
                                                                              y ev k
                                                                          else
                                                                              empty

(@->) :: (MonadPlus m) => SignalT s m Bool -> SignalT s m b -> SignalT s m b
SignalT ev0 x @-> SignalT ev1 y = SignalT (ev0 `Set.union` ev1) $ \ev k -> do
                                    x' <- x ev k
                                    if x' then y ev k else empty

stepper :: Applicative m => Event s -> SignalT s m Integer
stepper ev = triggerOn ev *> signalT (\_ k -> pure $ Map.findWithDefault 0 ev k)

stepperSum :: (Traversable f, Applicative m) => f (Event s) -> SignalT s m Integer
stepperSum evs = Fold.sum <$> traverse (stepper) evs

stepperOn :: Alternative m => SignalT s m a -> SignalT s m Integer
stepperOn (SignalT ev0 xx) = SignalT ev0 xx @> stepperSum (Set.toList ev0)
