module Sp.Test where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Sp.Eff
import Sp.Internal.Monad (HandleTag (HandleTag), Marker)
import Sp.Reader
import Sp.State

data SomeEff :: EffectH where
    SomeAction :: SomeEff m (SomeMarker '[])

data SomeMarker es = forall e. SomeMarker (Marker e es (SomeMarker es))

bad2 :: String
bad2 = runPure do
    SomeMarker mark <- interpret0 (\(HandleTag mark _) SomeAction -> pure $ SomeMarker mark) $ send SomeAction
    _ <- interpret0 (\(HandleTag mark' evv) SomeAction -> embed (HandleTag mark evv) $ pure $ SomeMarker mark') $ send SomeAction
    pure "a"

spTest :: IO ()
spTest = do
    print bad2
