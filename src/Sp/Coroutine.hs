module Sp.Coroutine where

import Sp.Eff

yield :: Yield a b :> ef => a -> Eff eh ef b
yield = send . Yield

data Yield a b :: EffectF where
    Yield :: a -> Yield a b b

data Status f a b r =
        Done r
    |   Continue a (b -> f (Status f a b r))

runCoroutine :: forall a b eh ef r. Eff '[] (Yield a b ': ef) r -> Eff eh ef (Status (Eff eh ef) a b r)
runCoroutine m = do
    interpret0 (\tag (Yield a) -> control tag \k -> pure $ Continue a (k . pure)) $ Done <$> m
