module Sp.Error where

import           Data.Kind (Type)
import           Sp.Eff
import Sp.Internal.Monad (abort)

-- | Allows you to throw error values of type @e@ and catching these errors too.
data Throw (e :: Type) :: EffectH where
  Throw :: e -> Throw e m a

data Try (e :: Type) :: EffectH where
  Try :: m a -> Try e m (Either e a)

-- | Throw an error.
throw :: (Throw e :> es, Monad m) => e -> Eff m es a
throw e = send (Throw e)

-- | Catch any error thrown by a computation and return the result as an 'Either'.
try :: (Try e :> es, Monad m) => Eff m es a -> Eff m es (Either e a)
try m = send (Try m)

-- | Catch any error thrown by a computation and handle it with a function.
catch :: (Try e :> es, Monad m) => Eff m es a -> (e -> Eff m es a) -> Eff m es a
catch m h = try m >>= either h pure

handleThrow :: ∀ e es m a. Monad m => Handler m (Throw e) es (Either e a)
handleThrow tag = \case
  Throw e -> abort tag (pure $ Left e)

handleTry :: ∀ e es m a. (Monad m, Throw e :> es) => Handler m (Try e) es (Either e a)
handleTry tag = \case
  Try m   -> replace (handleThrow @e) (Right <$> m)

-- | Run the 'Error' effect. If there is any unhandled error, it is returned as a 'Left'.
runThrow :: ∀ e es m a. Monad m => Eff m (Throw e : es) a -> Eff m es (Either e a)
runThrow = interpret (handleThrow @e) . fmap Right
