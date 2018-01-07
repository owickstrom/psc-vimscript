-- Test module.
module Main where

import Prelude

import Control.Monad.Eff

-- | Should be taken from the Prelude instead.
foreign import addInt :: Int -> Int -> Int

foreign import data CMDLINE :: Effect

foreign import echo :: forall e. String -> Eff (cmdline :: CMDLINE | e) Unit

-- | A cool data structure.
data Array a = Nil | Cons a (Array a)

-- | Returns the length of an `Array`.
arrayLength :: forall a. Array a -> Int
arrayLength Nil = 0
arrayLength (Cons x xs) = arrayLength xs `addInt` 1

-- | Example array.
l :: Array Int
l = Cons 1 (Cons 2 (Cons 3 Nil))

fst3 :: forall a b c. a -> b -> c -> a
fst3 a b c =
  case a of
    x -> x

setName :: forall r. { name :: String | r } -> String -> { name :: String | r }
setName r n = r { name = n }

main = do
  echo "Hello"
  echo "world"
