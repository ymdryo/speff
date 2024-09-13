{-# LANGUAGE QuantifiedConstraints #-}
module Sp.Reader where

import           Data.Kind (Type)
import           Sp.Eff

-- | Provides an environment value of type @r@, and you can override it in a local scope.
data Reader (r :: Type) :: EffectH where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

-- | Obtain the environment value.
ask :: (Reader r :> es, Monad m) => Eff m es r
ask = send Ask

-- | Override the environment value in a local scope.
local :: (Reader r :> es, Monad m) => (r -> r) -> Eff m es a -> Eff m es a
local f m = send (Local f m)

handleReader :: Monad m => r -> Handler m (Reader r) es a
handleReader !r _ = \case
  Ask       -> pure r
  Local f m -> replace (handleReader $ f r) m

-- | Run the 'Reader' effect with an environment value.
runReader :: Monad m => r -> Eff m (Reader r : es) a -> Eff m es a
runReader r = interpret (handleReader r)
