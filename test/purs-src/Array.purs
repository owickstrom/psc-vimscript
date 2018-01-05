-- This is my custom Array module.
module Array where

-- | Should be taken from the Prelude instead.
foreign import addInt :: Int -> Int -> Int

-- | A cool data structure.
data Array a = Nil | Cons a (Array a)

-- | Returns the length of an `Array`.
arrayLength :: forall a. Array a -> Int
arrayLength = go 0
  where
    go n Nil = n
    go n (Cons x xs) = go (addInt n 1) xs

-- | Example array.
l :: Array Int
l = Cons 1 (Cons 2 (Cons 3 Nil))
