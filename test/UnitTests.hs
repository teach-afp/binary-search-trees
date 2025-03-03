{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.BST.Internal

import Data.Bifunctor        (bimap)
import Data.Foldable         (toList)

import Test.Tasty            (TestTree, defaultMain)
import Test.Tasty.HUnit      (testCase, assertEqual)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testCase "split" $ assertEqual prefix actual expect
  where
    prefix = ""
    actual = bimap toList toList $
             split "dog" $
             fromList ["cat", "dog", "tiger", "wolf"]
    expect = (["cat"], ["tiger", "wolf"])