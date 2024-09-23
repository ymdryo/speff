module Sp.Reader where

import           Sp.Eff


-- | Provides an environment value of type @r@, and you can override it in a local scope.
data Ask r :: EffectF where
    Ask :: Ask r r

-- | Obtain the environment value.
ask :: Ask r :> ef => Eff eh ef r
ask = send Ask

-- | Run the 'Ask' effect with an environment value.
runAsk :: forall r eh ef a. HFunctors eh => r -> Eff eh (Ask r ': ef) a -> Eff eh ef a
runAsk r = interpretRec0 \_ Ask -> pure r

-- | Run the 'Ask' effect with an environment value.
runAsk' :: forall r eh ef a. r -> Eff '[] (Ask r ': ef) a -> Eff eh ef a
runAsk' r = interpret0 \_ Ask -> pure r

data Local r :: EffectH where
    Local :: (r -> r) -> m a -> Local r m a

-- | Override the environment value in a local scope.
local :: Local r :> eh => (r -> r) -> Eff eh ef a -> Eff eh ef a
local f m = sendH (Local f m)

runLocal :: forall r eh ef a. (HFunctors eh, Ask r :> ef) => Eff (Local r ': eh) ef a -> Eff eh ef a
runLocal = interpretRec0H \tag (Local f m) -> embedH tag $ interposeRec @(Ask r) (\tag' Ask -> f <$> embed tag' (send Ask)) m

instance HFunctor (Local r) where
    hfmap phi (Local f m) = Local f (phi m)
    {-# INLINE hfmap #-}
