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

{-
instance HFunctor (Ask r) where
    hfmap _ Ask = Ask
-}

runAsk :: HFunctors es => r -> Eff es (Ask r ': ef) a -> Eff es ef a
runAsk r = interpret0 \_ Ask -> pure r

data Local r :: EffectH where
    Local :: (r -> r) -> m a -> Local r m a

runLocal :: forall r es ef a. (HFunctors es, Ask r :> ef) => Eff (Local r ': es) ef a -> Eff es ef a
runLocal = interpret0H \tag (Local f m) -> embedH tag $ interpose0 @(Ask r) (\tag' Ask -> f <$> embed tag' (send Ask)) m

instance HFunctor (Local r) where
    hfmap phi (Local f m) = Local f (phi m)

data State s :: EffectF where
    Get :: State s s
    Put :: s -> State s ()

{-
instance HFunctor (State s) where
    hfmap _ = \case
        Get -> Get
        Put s -> Put s
-}

runState :: HFunctors eh => s -> Eff eh (State s ': ef) a -> Eff eh ef a
runState s m = runAsk s . interpret0 (\tag -> \case
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

data Yield a b :: EffectH where
    Yield :: a -> Yield a b m b

data Status f a b r = Done r | Coroutine a (b -> f (Status f a b r))

instance HFunctor (Yield a b) where
    hfmap _ (Yield x) = Yield x

runCoroutine :: forall a b es ef r. HFunctors es => Eff (Yield a b ': es) ef r -> Eff es ef (Status (Eff es ef) a b r)
runCoroutine m = do
    interpret0H (\tag (Yield a) -> control tag \k -> pure $ Coroutine a (k . pure)) $ Done <$> m


spTest :: IO ()
spTest = runIOE . runState "A" $ do
    s0 :: String <- send Get
    send $ Put $ s0 <> "B"
    liftIO . print @String =<< send Get
