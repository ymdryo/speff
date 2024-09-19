module Sp.Test where

import Control.Monad.IO.Class (liftIO)
import Sp.Eff
import Sp.Internal.Monad (unsafeIO)

data Yield a b :: Effect where
    Yield :: a -> Yield a b m b

data Status f a b r = Done r | Coroutine a (b -> f (Status f a b r))

runCoroutine :: forall a b es r. Eff (Yield a b ': es) r -> Eff es (Status (Eff es) a b r)
runCoroutine m = do
    interpret0 (\tag (Yield a) -> control tag \k -> pure $ Coroutine a (k . pure)) $ Done <$> m

spTest :: IO ()
spTest = runIOE do
    stat <- runCoroutine @Int @Int do
        r <- send $ Yield @Int @Int 0
        unsafeIO $ putStrLn $ "reply: " <> show r
        _ <- send $ Yield @Int @Int 100
        pure ()

    case stat of
        Done n -> unsafeIO $ print n
        Coroutine n k -> do
            unsafeIO $ putStrLn $ "yielded: " <> show n
            stat' <- k 20
            case stat' of
                Done () -> liftIO $ putStrLn "Done."
                Coroutine n' _ -> liftIO $ putStrLn $ "Continue... " <> show n'
            pure ()
