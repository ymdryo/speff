module Sp.Test where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Sp.Eff
import Sp.Internal.Monad (HandleTag (HandleTag), Marker, abort)
import Sp.Reader
import Sp.State

data Yield a b :: Effect where
    Yield :: a -> Yield a b b

data Status f a b r = Done r | Coroutine a (b -> f (Status f a b r))

runCoroutine :: forall a b es m r. Monad m => Eff m (Yield a b ': es) r -> Eff m es (Status (Eff m es) a b r)
runCoroutine m = do
    interpret0 (\tag (Yield a) -> control tag \k -> pure $ Coroutine a k) $ Done <$> m

spTest :: IO ()
spTest = runEff do
    undefined
