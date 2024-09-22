module Sp.Coroutine where

import Sp.Eff

data Yield a b :: Effect where
    Yield :: a -> Yield a b m b

data Status es a b r =
      Done r
    | Continue a (b -> Eff es (Status es a b r))

yield :: Yield a b :> es => a -> Eff es b
yield = send . Yield

runCoroutine :: forall a b es r. Eff (Yield a b ': es) r -> Eff es (Status es a b r)
runCoroutine m = do
    interpret0 (\tag (Yield a) -> control tag \k -> pure $ Continue a (k . pure)) $ Done <$> m
