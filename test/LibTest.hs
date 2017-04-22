module LibTest where

import Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Lib
import qualified Set1.Ch3 as Ch3

hexTest = testCase "fromHex" (fromHex "ffFF" @=? BS.pack [255,255])

englishInput :: BS.ByteString
englishInput = read "\"Now that the party is jumping\n\""
englishTest = testCase "englishLike"
  $ englishLike (freq englishInput) @=? True

englishInput2 :: BS.ByteString
englishInput2 = read "\"Cooking MC's like a pound of bacon\""
englishTest2 = testCase "englishLike"
  $ englishLike (freq englishInput2) @=? True

utils = testGroup "Utilities" [hexTest, englishTest, englishTest2]
