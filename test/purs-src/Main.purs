-- Test module.
module Main where

import Prelude

import Control.Monad.Eff (kind Effect, Eff, foreachE)

foreign import data CMDLINE :: Effect

foreign import echo :: forall e. String -> Eff (cmdline :: CMDLINE | e) Unit

main = foreachE ["Hello", "world", "I", "am", "PureScript"] echo
