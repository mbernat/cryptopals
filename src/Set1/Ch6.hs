module Set1.Ch6 where

import qualified Data.ByteString as BS

import Lib

keySizeDiff :: BS.ByteString -> Int -> Float
keySizeDiff s n = fromIntegral (hamming a b) / (fromIntegral n)
  where
    (a, b) = BS.splitAt n $ BS.drop (2*n) s
