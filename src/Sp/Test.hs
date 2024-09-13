{-# LANGUAGE ImportQualifiedPost #-}

module Sp.Test where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)
import Debug.Trace (trace)
import Sp.Eff
import Sp.Internal.Env qualified as Rec
import Sp.Internal.Monad (Evidence (Evidence), HandleTag (HandleTag), Marker (Marker), under, withEvidence, withEvv)
import Sp.Reader
import Sp.State

handleStatePure :: forall s es m a. (Monad m, Show s) => Handler m (State s) (Reader s ': es) a
handleStatePure tag@(HandleTag mark evv) = \case
    Get -> do
        trace ("mark = " ++ show mark) $ pure ()
        under mark evv do
            s <- (\(Evidence mark' evv'' h) -> trace ("mark' = " ++ show mark') $ h (HandleTag mark' evv'') Ask) $ Rec.index @(Reader s) evv
            trace ("s = " ++ show s) $ pure s
    Put s ->
        flip control tag \k -> do
            lift1 $ runReader s $ k ()

runStatePure :: (MonadIO m, Show s) => s -> Eff m (State s : Reader s ': es) a -> Eff m es (a, s)
runStatePure s0 m =
    runReader s0 do
        interpret0 handleStatePure do
            r <- m
            s <- ask
            pure (r, s)

get :: (State Integer :> es, MonadIO m) => Eff m es Integer
get = send Get

put :: (State Integer :> es, MonadIO m) => Integer -> Eff m es ()
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
