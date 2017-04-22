module Set1.Ch4 where

import Data.Maybe
import qualified Data.List as List

import qualified Data.ByteString as BS

import Lib
import qualified Set1.Ch3 as Ch3

input :: IO [String]
input = lines <$> readFile "4.txt"

isEnglish :: String -> Bool
isEnglish = (/= []) . Ch3.candidateXor . fromHex

solve :: [String] -> BS.ByteString
solve lines = Ch3.decrypt line
  where
    line = fromJust $ List.find isEnglish lines


