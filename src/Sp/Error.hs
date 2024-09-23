module Sp.Error where

import           Data.Kind (Type)
import           Sp.Eff
import Control.Exception (throwIO, Exception)

-- | Allows you to throw error values of type @e@ and catching these errors too.
data Throw (e :: Type) :: EffectF where
  Throw :: e -> Throw e a

data Try (e :: Type) :: EffectH where
  Try :: m a -> Try e m (Either e a)

instance HFunctor (Try e) where
    hfmap f (Try m) = Try $ f m
    {-# INLINE hfmap #-}

-- | Throw an error.
throw :: Throw e :> ef => e -> Eff eh ef a
throw e = send (Throw e)

-- | Catch any error thrown by a computation and return the result as an 'Either'.
try :: Try e :> eh => Eff eh ef a -> Eff eh ef (Either e a)
try m = sendH (Try m)

-- | Catch any error thrown by a computation and handle it with a function.
catch :: Try e :> eh => Eff eh ef a -> (e -> Eff eh ef a) -> Eff eh ef a
catch m h = try m >>= either h pure

handleThrow :: ∀ e eh ef a. Handler (Throw e) eh ef (Either e a)
handleThrow tag = \case
  Throw e -> abort tag (pure $ Left e)

elabTry :: forall e ef a. (Throw e :> ef) => Elaborator (Try e) '[] ef a
elabTry tag = \case
  Try m -> embedH tag $ interpose (handleThrow @e) (Right <$> m)

runThrow :: ∀ e eh ef a. Eff '[] (Throw e ': ef) a -> Eff eh ef (Either e a)
runThrow = interpret (handleThrow @e) . fmap Right

runTry :: ∀ e ef a. Throw e :> ef => Eff '[Try e] ef a -> Eff '[] ef a
runTry = interpretRecH (elabTry @e)

