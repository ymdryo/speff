{-# LANGUAGE PackageImports #-}
module Sp.Test where

import "hefty-freer-simple" Control.Monad.Freer
import "hefty-freer-simple" Control.Monad.Freer.Coroutine
import "hefty-freer-simple" Control.Monad.Freer.State
import "hefty-freer-simple" Control.Monad.Freer.Reader
import Control.Monad.IO.Class (liftIO)

coroutine :: IO ()
coroutine = runM do
    stat <- runC @Int @Int do
        r <- yield @Int @Int 0 id
        liftIO $ putStrLn $ "reply: " <> show r
        _ <- yield @Int @Int 100 id
        pure ()

    case stat of
        Done n -> liftIO $ print n
        Continue n k -> do
            liftIO $ putStrLn $ "yielded: " <> show n
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
spTestOk = runM . runReader @Int 0 . fmap fst . runState "A" . runLocal @Int $ do
    s0 :: String <- send Get
    sendH $ Local @Int (+1) do
        send $ Put $ s0 <> "B"
        liftIO . print @Int =<< send Ask
    liftIO . print @String =<< send Get

spTestRec :: IO ()
spTestRec = runM . runReader @Int 0 . runLocal @Int . runStateRec "A" $ do
    s0 :: String <- send Get
    send $ Put $ s0 <> "C"
    sendH $ Local @Int (+1) do
        send $ Put $ s0 <> "B"
        liftIO . print @Int =<< send Ask
        liftIO . print @String =<< send Get
    liftIO . print @String =<< send Get

