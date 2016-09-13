module Graphics.Util (($>), guardA) where

import Control.Applicative

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

guardA :: Alternative f => Bool -> f ()
guardA True = pure ()
guardA False = empty
