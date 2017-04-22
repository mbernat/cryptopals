module Set1Test where

import Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Lib
import qualified Set1.Ch3 as Ch3
import qualified Set1.Ch4 as Ch4

s1ch1_input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

s1ch1_output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

ch1 = testCase "Challenge 1" 
  $ show (base64 (fromHex s1ch1_input)) @=? show s1ch1_output

s1ch2_input1 = "1c0111001f010100061a024b53535009181c"

s1ch2_input2 = "686974207468652062756c6c277320657965"

s1ch2_output = "746865206b696420646f6e277420706c6179"

ch2 = testCase "Challenge 2"
  $ show (fullXor (fromHex s1ch2_input1) (fromHex s1ch2_input2))
  @=? show (fromHex s1ch2_output)

s1ch3_input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
s1ch3_output = "Cooking MC's like a pound of bacon"

ch3 = testCase "Challenge 3"
  $ show (Ch3.decrypt s1ch3_input) @=? show s1ch3_output

s1ch4_output = "Now that the party is jumping\n"

testCh4 = do
  lines <- Ch4.input
  let res = Ch4.solve lines
  assertBool "What" $ show res == show s1ch4_output

ch4 = testCase "Challenge 4" testCh4

s1 = testGroup "Set 1" [ch1, ch2, ch3, ch4]

