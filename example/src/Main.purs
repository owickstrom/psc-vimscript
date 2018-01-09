-- Test module.
module Main where

import Prelude

import Control.Monad.Eff (kind Effect, Eff, foreachE)
import Data.Traversable (traverse, for, for_)


foreign import data VIM :: Effect

foreign import echo :: forall e. String -> Eff (vim :: VIM | e) Unit

foreign import input :: forall e. String -> Eff (vim :: VIM | e) String

foreign import confirm :: forall e. String -> Eff (vim :: VIM | e) Boolean

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

askAbout sub = do
  let q = "How about " <> sub <> "? "
  a <- input q
  pure { subject: sub, answer: a }

main = do
  for_ ["Hello", "world", "I", "am", "PureScript"] echo
  loop 10
  echo (greet ["Alice"])
  echo (greet ["Bob", "Carol"])
  echo (greet ["Darlene", "Bob", "Carol"])

  echo (greet' { name: "Alice", age: 25 })
  echo (greet' { name: "Bob", age: 80 })
  echo (greet' { name: "Carol", age: 17 })

  questions <- traverse askAbout ["Vim", "Emacs", "Haskell", "PureScript"]
  echo "Thank you."
  for_ questions do \q -> echo ("About " <> q.subject <> ", you said: " <> q.answer)
