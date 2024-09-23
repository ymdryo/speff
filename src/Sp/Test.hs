module Sp.Test where

import Sp.Eff
import Control.Monad.IO.Class (liftIO)

{-

-- rather `effTest`.
-- coroutine semantics test for `eff`.
spTest :: IO ()
spTest = runIOE do
    stat <- runCoroutine @Int @Int do
        r <- sendH $ Yield @Int @Int 0
        unsafeIO $ putStrLn $ "reply: " <> show r
        _ <- sendH $ Yield @Int @Int 100
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
-}

data Ask r :: EffectF where
    Ask :: Ask r r

runAsk :: forall r eh ef a. HFunctors eh => r -> Eff eh (Ask r ': ef) a -> Eff eh ef a
runAsk r = interpretRec0 \_ Ask -> pure r

data Local r :: EffectH where
    Local :: (r -> r) -> m a -> Local r m a

runLocal :: forall r eh ef a. (HFunctors eh, Ask r :> ef) => Eff (Local r ': eh) ef a -> Eff eh ef a
runLocal = interpretRec0H \tag (Local f m) -> embedH tag $ interposeRec @(Ask r) (\tag' Ask -> f <$> embed tag' (send Ask)) m

instance HFunctor (Local r) where
    hfmap phi (Local f m) = Local f (phi m)

data State s :: EffectF where
    Get :: State s s
    Put :: s -> State s ()

runState :: HFunctors eh => s -> Eff eh (State s ': ef) a -> Eff eh ef a
runState s m = runAsk s . interpretRec0 (\tag -> \case
        Get -> embed tag $ send Ask
        Put s' -> control tag \k -> lift1 $ runAsk s' $ k $ pure ()
    ) $ lift1Under1 m

data Throw e :: EffectH where
    Throw :: e -> Throw e m a

instance HFunctor (Throw e) where
    hfmap _ (Throw e) = Throw e

data Try e :: EffectH where
    Try :: m a -> Try e m (Either e a)

instance HFunctor (Try e) where
    hfmap f (Try m) = Try $ f m

data Yield a b :: EffectF where
    Yield :: a -> Yield a b b

data Status f a b r = Done r | Coroutine a (b -> f (Status f a b r))

runCoroutine :: forall a b eh ef r. Eff '[] (Yield a b ': ef) r -> Eff eh ef (Status (Eff eh ef) a b r)
runCoroutine m = do
    interpret0 (\tag (Yield a) -> control tag \k -> pure $ Coroutine a (k . pure)) $ Done <$> m

spTest :: IO ()
spTest = do
    putStrLn "[OK case]"
    spTestOk

    putStrLn "[Recursive-Reset Interpretation]"
    spTestRec

spTestOk :: IO ()
spTestOk = runIOE . runAsk @Int 0 . runState "A" . runLocal @Int $ do
    s0 :: String <- send Get
    sendH $ Local @Int (+1) do
        send $ Put $ s0 <> "B"
        liftIO . print @Int =<< send Ask
    liftIO . print @String =<< send Get

spTestRec :: IO ()
spTestRec = runIOE . runAsk @Int 0 . runLocal @Int . runState "A" $ do
    s0 :: String <- send Get
    sendH $ Local @Int (+1) do
        send $ Put $ s0 <> "B"
        liftIO . print @Int =<< send Ask
        liftIO . print @String =<< send Get
    liftIO . print @String =<< send Get

