module LibTest where

import Data.String

import Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Lib

hexTest = testCase "fromHex" (fromHex "ffFF" @=? BS.pack [255,255])

englishInput :: BS.ByteString
englishInput = read "\"Now that the party is jumping\n\""
englishTest = testCase "englishLike"
  $ englishLike (freq englishInput) @?= True

englishInput2 :: BS.ByteString
englishInput2 = read "\"Cooking MC's like a pound of bacon\""
englishTest2 = testCase "englishLike"
  $ englishLike (freq englishInput2) @?= True

hammingTest = testCase "hamming"
  $ hamming (fromString "this is a test") (fromString "wokka wokka!!!") @?= 37

utils = testGroup "Utilities"
  [ hexTest
  , englishTest
  , englishTest2
  , hammingTest
  ]
