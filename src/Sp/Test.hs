module Sp.Test where

import Control.Effect (runCoroutine, yield, run, Coroutine, Status(..), type (:<), Eff)
import Control.Monad (forM)

programEff :: Coroutine Int Int :< es => Eff es [Int]
programEff = forM [1..100] \i -> yield @Int @Int i

loopStatusEff :: Status es Int Int r -> Eff es r
loopStatusEff = \case
    Done r -> pure r
    Yielded i f -> loopStatusEff =<< runCoroutine (f (i+100))

-- rather `effTest`.
-- coroutine semantics test for `eff`.
spTest :: IO ()
spTest = do
    print @[Int] $ run do
        s <- runCoroutine @Int @Int programEff
        loopStatusEff s
