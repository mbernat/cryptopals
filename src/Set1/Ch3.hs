module Set1.Ch3 where

import Data.Word
import qualified Data.List as List

import qualified Data.ByteString as BS

import Lib

xorOne :: BS.ByteString -> Word8 -> BS.ByteString
xorOne s c = fullXor s (BS.replicate (BS.length s) c)

candidateXor :: BS.ByteString -> [Word8]
candidateXor s =
  List.map fst . List.filter englishLike' $ List.map xorFreq [0..255]
  where
    xorFreq n = (n, freq $ xorOne s n)

    englishLike' = englishLike . snd

decrypt :: String -> BS.ByteString
decrypt input = xorOne s (head $ candidateXor s)
  where
    s = fromHex input

input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
