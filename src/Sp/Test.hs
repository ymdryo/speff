module Sp.Test where

import Data.Kind (Type)
import Sp.Eff
import Sp.Reader
import Sp.State

handleStatePure :: forall s es a. Reader s :> es => Handler (State s) es a
handleStatePure tag = \case
    Get ->
        control tag \k -> do
            s <- ask
            k $ pure s
    Put s ->
        control tag \k -> do
            local (const s) $ k $ pure ()
    _ -> error "not implemented"

runStatePure :: s -> Eff (State s : es) a -> Eff es (a, s)
runStatePure s0 m =
    runReader s0 do
        interpret0 handleStatePure do
            r <- liftUnder1 m
            s <- ask
            pure (r, s)

spTest :: IO ()
spTest = print @(Integer, Integer) $ runEff do
    runStatePure 10 do
        get @Integer >>= put . (+ 5)
        n <- get
        get @Integer >>= put . (+ 5)
        get @Integer >>= put . (+ 5)
        pure n
