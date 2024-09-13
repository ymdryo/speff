module Sp.Test where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Sp.Eff
import Sp.Internal.Monad (HandleTag (HandleTag), Marker, abort)
import Sp.Reader
import Sp.State

data Yield a b :: EffectH where
    Yield :: a -> Yield a b m b

data Status f a b r = Done r | Coroutine a (b -> f (Status f a b r))

runCoroutine :: forall a b es m r. Monad m => Eff m (Yield a b ': es) r -> Eff m es (Status (Eff m es) a b r)
runCoroutine m = do
    interpret0 (\tag (Yield a) -> control tag \k -> pure $ Coroutine a k) $ Done <$> m

spTest :: IO ()
spTest = runEff do
    stat <- runCoroutine @Int @Int do
        r <- send $ Yield @Int @Int 0
        liftIO $ putStrLn $ "reply: " <> show r
        _ <- send $ Yield @Int @Int 100
        pure ()

    case stat of
        Done n -> liftIO $ print n
        Coroutine n k -> do
            liftIO $ putStrLn $ "yielded: " <> show n
            stat' <- k 20
            case stat' of
                Done () -> liftIO $ putStrLn "Done."
                Coroutine n' _ -> liftIO $ putStrLn $ "Continue... " <> show n'
            pure ()
