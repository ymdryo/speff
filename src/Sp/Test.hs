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

data Ask r :: EffectH where
    Ask :: Ask r m r

instance HFunctor (Ask r) where
    hfmap _ Ask = Ask

runAsk :: forall r es ef a. HFunctors es => r -> Eff (Ask r ': es) ef a -> Eff es ef a
runAsk r = interpret0H \_ Ask -> pure r

data Local r :: EffectH where
    Local :: (r -> r) -> m a -> Local r m a

runLocal :: forall r es ef a. (HFunctors es, Ask r :> es) => Eff (Local r ': es) ef a -> Eff es ef a
runLocal = interpret0H \tag (Local f m) -> embedH tag $ replace0H @(Ask r) (\tag' Ask -> f <$> embedH tag' (sendH Ask)) m

instance HFunctor (Local r) where
    hfmap phi (Local f m) = Local f (phi m)

data State s :: EffectH where
    Get :: State s m s
    Put :: s -> State s m ()

instance HFunctor (State s) where
    hfmap _ = \case
        Get -> Get
        Put s -> Put s

runState :: HFunctors eh => s -> Eff (State s ': eh) ef a -> Eff eh ef a
runState s m = runAsk s . interpret0H (\tag -> \case
        Get -> embedH tag $ sendH Ask
        Put s' -> control tag \k -> lift1H $ runAsk s' $ k $ pure ()
    ) $ lift1Under1H m

data Throw e :: EffectH where
    Throw :: e -> Throw e m a

instance HFunctor (Throw e) where
    hfmap _ (Throw e) = Throw e

data Try e :: EffectH where
    Try :: m a -> Try e m (Either e a)

instance HFunctor (Try e) where
    hfmap f (Try m) = Try $ f m

data Yield a b :: EffectH where
    Yield :: a -> Yield a b m b

data Status f a b r = Done r | Coroutine a (b -> f (Status f a b r))

instance HFunctor (Yield a b) where
    hfmap _ (Yield x) = Yield x

runCoroutine :: forall a b es ef r. HFunctors es => Eff (Yield a b ': es) ef r -> Eff es ef (Status (Eff es ef) a b r)
runCoroutine m = do
    interpret0H (\tag (Yield a) -> control tag \k -> pure $ Coroutine a (k . pure)) $ Done <$> m

spTest :: IO ()
spTest = do
    putStrLn "[OK case]"
    spTestOk

    putStrLn "[NG case]"
    spTestRuntimeError

spTestOk :: IO ()
spTestOk = runIOE . runAsk @Int 0 . runState "A" . runLocal @Int $ do
    s0 :: String <- sendH Get
    sendH $ Local @Int id do
        sendH $ Put $ s0 <> "B"
    liftIO . print @String =<< sendH Get

-- Runtime Error: "Sp.Internal.Vec: uninitialized element"
--
-- This is an expected result (a general principle from Hefty Algebra/Heftia): Without relying on "reset-based"
-- recursive interpretation via `HFunctor`, it is not possible to interpret first-order effects before higher-order
-- effects. We cannot maintain continuational state beyond a scope of uninterpreted higher-order effects.
spTestRuntimeError :: IO ()
spTestRuntimeError = runIOE . runAsk @Int 0 . runLocal @Int . runState "A" $ do
    s0 :: String <- sendH Get
    sendH $ Local @Int id do
        sendH $ Put $ s0 <> "B"
    liftIO . print @String =<< sendH Get
