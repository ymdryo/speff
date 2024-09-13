module Sp.Test where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Sp.Eff
import Sp.Error
import Sp.Reader
import Sp.State

data SomeEff :: EffectH where
    SomeAction :: SomeEff m String

bad1 :: Either String String
bad1 = runPure . runError @String $ do
    interpret0 (\tag SomeAction -> embed tag $ throwError "not caught") $ do
        send SomeAction `catchError` \(_ :: String) -> return "caught"

bad2 :: String
bad2 = runPure . runReader "unlocaled" $ do
    interpret0 (\tag SomeAction -> embed tag ask) $ do
        local (\_ -> "localed") $ send SomeAction

spTest :: IO ()
spTest = do
    print bad1
    print bad2
