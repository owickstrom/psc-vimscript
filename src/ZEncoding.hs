{-# LANGUAGE OverloadedStrings #-}
{-
 - Implements Z-encoding for name mangling.
 -
 - This is mostly taken from the `zenc` package, which contains code from
 - the GHC source.
 -
 - https://hackage.haskell.org/package/zenc
 -}
module ZEncoding
  ( zEncode
  ) where

import           Data.Char
import           Numeric
import           Data.Text                        (Text)
import qualified Data.Text                        as T

isUnencoded :: Char -> Bool -- True for chars that don't need encoding
isUnencoded 'Z' = False
isUnencoded 'z' = False
isUnencoded c   = isAsciiLower c || isAsciiUpper c || isDigit c

zEncode :: Text -> Text
zEncode = T.concatMap go
  where
    go c
      | isDigit c = encodeUnicode c
    go c = encodeCh c

encodeCh :: Char -> Text
encodeCh c
  | isUnencoded c = T.singleton c
-- encodeCh '('  = "ZL"
-- encodeCh ')'  = "ZR"
-- encodeCh '['  = "ZM"
-- encodeCh ']'  = "ZN"
encodeCh c =
  case c of
    ':'  -> "ZC" -- Colon
    'Z'  -> "ZZ"
    'z'  -> "zz"
    '&'  -> "za" -- And
    '|'  -> "zb"
    '^'  -> "zc" -- Caret
    '$'  -> "zd" -- Dollar
    '='  -> "ze" -- Equals
    '>'  -> "zg" -- Greater than
    '#'  -> "zh" -- Hash
    '.'  -> "zi" -- ... there's a dot on top of an 'i'?
    '<'  -> "zl" -- Less than
    '-'  -> "zm" -- eM-dash?
    '!'  -> "zn"
    '+'  -> "zp" -- Plus
    '\'' -> "zq" -- Quote?
    '\\' -> "zr"
    '/'  -> "zs" -- Slash
    '*'  -> "zt" -- Times
    '_'  -> "zu" -- Underscore
    '%'  -> "zv"
    _    -> encodeUnicode c

encodeUnicode :: Char -> Text
encodeUnicode c =
  T.cons 'z' $
  if isDigit (T.head hexStr)
    then hexStr
    else T.cons '0' hexStr
  where
    hexStr = T.pack (showHex (ord c) "U")
