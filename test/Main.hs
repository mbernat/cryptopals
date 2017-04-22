module Main where

import Test.Tasty

import LibTest
import Set1Test

allTests = testGroup "Everything" [utils, s1]

main = defaultMain allTests
