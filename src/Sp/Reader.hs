{-# LANGUAGE QuantifiedConstraints #-}
module Sp.Reader where

import           Data.Kind (Type)
import           Sp.Eff

-- | Provides an environment value of type @r@, and you can override it in a local scope.
data Reader (r :: Type) :: Effect where
  Ask :: Reader r r

-- | Obtain the environment value.
ask :: (Reader r :> es) => Eff es r
ask = send Ask

handleReader :: r -> Handler  (Reader r) es a
handleReader !r _ = \case
  Ask       -> pure r

-- | Run the 'Reader' effect with an environment value.
runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = interpret (handleReader r)
