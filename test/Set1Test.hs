module Set1Test where

import Data.String

import Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Lib
import qualified Set1.Ch3 as Ch3
import qualified Set1.Ch4 as Ch4
import qualified Set1.Ch6 as Ch6

ch1_input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

ch1_output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

ch1 = testCase "Challenge 1" 
  $ show (base64 (fromHex ch1_input)) @?= show ch1_output

ch2_input1 = "1c0111001f010100061a024b53535009181c"

ch2_input2 = "686974207468652062756c6c277320657965"

ch2_output = "746865206b696420646f6e277420706c6179"

ch2 = testCase "Challenge 2"
  $ show (fullXor (fromHex ch2_input1) (fromHex ch2_input2))
  @?= show (fromHex ch2_output)

ch3_input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
ch3_output = "Cooking MC's like a pound of bacon"

ch3 = testCase "Challenge 3"
  $ show (Ch3.decrypt ch3_input) @?= show ch3_output

ch4_output = "Now that the party is jumping\n"

testCh4 = do
  lines <- Ch4.input
  let res = Ch4.solve lines
  assertBool "What" $ show res == show ch4_output

ch4 = testCase "Challenge 4" testCh4

ch5_input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
ch5_output = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

heXor s k = hex $ repXor (fromString s) (fromString k)

testCh5 = (heXor ch5_input "ICE") @?= fromString ch5_output

ch5 = testCase "Challenge 5" testCh5

keySizeDiff_input = "a very interesting text"


s1 = testGroup "Set 1" [ch1, ch2, ch3, ch4, ch5]
