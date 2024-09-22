module Sp.Test where

import Control.Effect
import Control.Effect.Coroutine
import Control.Monad (forM)

programEff :: Coroutine Int Int :< es => Eff es [Int]
programEff = forM [1..100] \i -> yield @Int @Int i

loopStatusEff :: Status es Int Int r -> Eff es r
loopStatusEff = \case
    Done r -> pure r
    Yielded i f -> loopStatusEff =<< runCoroutine (f (i+100))

spTest :: IO ()
spTest = do
    print @[Int] $ run do
        s <- runCoroutine @Int @Int programEff
        loopStatusEff s
