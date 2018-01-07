-- Test module.
module Main where

import Prelude

import Control.Monad.Eff (kind Effect, Eff, foreachE)

foreign import data CMDLINE :: Effect

foreign import echo :: forall e. String -> Eff (cmdline :: CMDLINE | e) Unit

loop 0 = pure unit
loop n = do
  echo (show n)
  loop (n - 1)

main = do
  foreachE ["Hello", "world", "I", "am", "PureScript"] echo
  loop 10
