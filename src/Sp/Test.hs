module Sp.Test where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)
import Sp.Eff
import Sp.Internal.Monad (Evidence (Evidence), HandleTag (HandleTag), under, withEvidence, withSubContext)
import Sp.Reader
import Sp.State

handleStatePure :: forall s es m a. MonadIO m => Handler m (State s) (Reader s ': es) a
handleStatePure tag@(HandleTag mark evv) = \case
    Get ->
        {-
        flip control tag \k -> do
            r <- ask
            k r
        -}
        -- embed tag $ ask
        -- under mark evv $ withSubContext \(Evidence m' ctx' h) -> h (HandleTag m' ctx') Ask
        embed tag ask
    Put s ->
        flip control tag \k -> do
            lift1 $ runReader s $ k ()

runStatePure :: MonadIO m => s -> Eff m (State s : Reader s ': es) a -> Eff m es (a, s)
runStatePure s0 m =
    runReader s0 do
        interpret0 handleStatePure do
            r <- m
            s <- ask
            pure (r, s)

get :: (State Integer :> es, Monad m) => Eff m es Integer
get = send Get

put :: (State Integer :> es, Monad m) => Integer -> Eff m es ()
put x = send $ Put x

spTest :: IO ()
spTest =
    print @(Integer, Integer) =<< runEff do
        runStatePure 10 do
            {-
            get >>= put . (+ 20)
            n <- get
            get >>= put . (+ 30)
            get >>= put . (+ 40)
            pure n
            -}
            s <- get
            put $ s + 1
            s' <- get
            put $ s' + 1
            get
