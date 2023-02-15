{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.BST.Internal

import Data.Foldable         (toList)
import Data.Function         (on)
import Test.QuickCheck
import Test.Tasty            (TestTree, defaultMain)
import Test.Tasty.QuickCheck (testProperties)

instance (Arbitrary a, Ord a) => Arbitrary (BST a) where
  arbitrary = fromList <$> arbitrary

instance CoArbitrary a => CoArbitrary (BST a) where
  coarbitrary = coarbitrary . toList

-- | Extensional equality.

(~) :: Ord a => BST a -> BST a -> Bool
(~) = (==) `on` toList

-- Some properties of sets.

prop_insert      x s = member x (insert x s)
prop_delete      x s = not $ member x (delete x s)
prop_union_split x s = uncurry union (split x s) ~ delete x s

return []

tests :: TestTree
tests = testProperties "Tests" $allProperties

main :: IO ()
main = defaultMain tests
