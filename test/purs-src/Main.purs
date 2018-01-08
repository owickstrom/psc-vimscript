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

greet who =
  case who of
       [one] -> "Hi, " <> one <> "!"
       [one, other] -> "Hi, " <> one <> " and " <> other <> "!"
       _ -> "Hi, y'all!"

greet' who =
  case who of
       { name: "Alice" } -> "Hi, Alice!"
       { age: 18 } -> "Want a beer?"
       {age} | age > 70 -> "Howdy, old timer!"
       _ -> "Hey, you."

main = do
  foreachE ["Hello", "world", "I", "am", "PureScript"] echo
  loop 10
  echo (greet ["Alice"])
  echo (greet ["Bob", "Carol"])
  echo (greet ["Darlene", "Bob", "Carol"])

  echo (greet' { name: "Alice", age: 25 })
  echo (greet' { name: "Bob", age: 80 })
  echo (greet' { name: "Carol", age: 17 })
