module Lib
where

import Data.Bits
import Data.Char
import Data.List as List
import Data.Ord
import Data.String
import Data.Word
import Numeric


import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map

hex :: BS.ByteString -> BS.ByteString
hex = BS.concatMap (fromString . wordHex)
  where
    wordHex n = showHex (n `div` 16) $ showHex (n `mod` 16) ""

fromHex' :: String -> BS.ByteString
fromHex' = BS.pack . List.map toWord . Split.chunksOf 2
  where
    toWord :: String -> Word8
    toWord [a, b] = (hex a) * 16 + (hex b)
    toWord _ = error "Cannot convert single hexadecimal character to a byte."
    hex c
      | c >= '0' && c <= '9' = fromIntegral $ (ord c) - 48
      | c >= 'a' && c <= 'f' = fromIntegral $ (ord c) - 87
      | c >= 'A' && c <= 'F' = fromIntegral $ (ord c) - 55
      | otherwise = error "Not a hexadecimal character."
    
fromHex = BS.pack . List.map toWord . Split.chunksOf 2
  where
    toWord = fromIntegral . fst . List.head . readHex

base64 :: BS.ByteString -> BS.ByteString
base64 = Base64.encode

fullXor :: BS.ByteString -> BS.ByteString -> BS.ByteString
fullXor a b = BS.pack . List.map (uncurry xor) $ BS.zip a b

hamming :: BS.ByteString -> BS.ByteString -> Int
hamming a b = List.sum . List.map (popCount . uncurry xor) $ BS.zip a b

repXor :: BS.ByteString -> BS.ByteString -> BS.ByteString
repXor s k = fullXor s rk
  where
    rk = BS.take l . BS.concat $ replicate l k
    l = BS.length s




counts :: BS.ByteString -> Map.Map Char Int
counts = BS.foldl' adjust' zeroMap
  where
    adjust' m k = Map.adjust (+1) (toLower . chr $ fromIntegral k) m
    zeroMap :: Map.Map Char Int
    zeroMap = List.foldl' insert' Map.empty [0..255]
    insert' m k = Map.insert (chr $ fromIntegral k) 0 m

freq :: BS.ByteString -> [(Char, Int)]
freq =
  List.filter pos . List.reverse . sortBy (comparing snd) . Map.toList . counts
  where
    pos = (>0) . snd

common c = elem c " etaoinshrdlu"

topCommon :: [(Char, Int)] -> Bool
topCommon l = length l > 1 &&
  (List.all (common . fst) $ List.filter ((>= 4) . snd) l)

count' = fromIntegral . List.sum . List.map snd

countCommon = count' . List.filter (common . fst)

mostEnglish :: [(Char, Int)] -> Bool
mostEnglish l = (countCommon l) > (count' l * 0.6)

englishLike l = mostEnglish l && topCommon l

keepPosAndIndex :: (Num a, Ord a) => [a] -> [(Int, a)]
keepPosAndIndex l = filter ((>0) . snd) $ zip [0..] l
