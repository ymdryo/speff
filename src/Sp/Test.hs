module Sp.Test where

import Sp.Eff
import Control.Monad.IO.Class (liftIO)
import Sp.Coroutine
import Sp.State
import Sp.Reader

-- rather `effTest`.
-- coroutine semantics test for `eff`.
coroutine :: IO ()
coroutine = runIOE do
    stat <- runCoroutine @Int @Int do
        r <- send $ Yield @Int @Int 0
        unsafeIO $ putStrLn $ "reply: " <> show r
        _ <- send $ Yield @Int @Int 100
        pure ()

    case stat of
        Done n -> unsafeIO $ print n
        Continue n k -> do
            unsafeIO $ putStrLn $ "yielded: " <> show n
            stat' <- k 20
            case stat' of
                Done () -> liftIO $ putStrLn "Done."
                Continue n' _ -> liftIO $ putStrLn $ "Continue... " <> show n'
            pure ()

spTest :: IO ()
spTest = do
    putStrLn "[OK case]"
    spTestOk

    putStrLn "[Recursive-Reset Interpretation]"
    spTestRec

    putStrLn "[coroutine]"
    coroutine

spTestOk :: IO ()
spTestOk = runIOE . runAsk @Int 0 . fmap fst . runStatePure "A" . runLocal @Int $ do
    s0 :: String <- send Get
    sendH $ Local @Int (+1) do
        send $ Put $ s0 <> "B"
        liftIO . print @Int =<< send Ask
    liftIO . print @String =<< send Get

spTestRec :: IO ()
spTestRec = runIOE . runAsk @Int 0 . runLocal @Int . fmap fst . runStatePureRec "A" $ do
    s0 :: String <- send Get
    sendH $ Local @Int (+1) do
        send $ Put $ s0 <> "B"
        liftIO . print @Int =<< send Ask
        liftIO . print @String =<< send Get
    liftIO . print @String =<< send Get

