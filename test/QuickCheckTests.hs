{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.BST.Internal

import Data.Foldable         (toList)
import Data.Function         (on)
import Test.QuickCheck
import Test.Tasty            (TestTree, defaultMain)
import Test.Tasty.QuickCheck (testProperties)

instance (Arbitrary a, Ord a) => Arbitrary (BST a) where

  -- Generate random BSTs.
  arbitrary :: Gen (BST a)
  arbitrary = fromList <$> arbitrary

  -- Shrink a tree to smaller trees, to reduce counterexamples.
  shrink :: BST a -> [BST a]
  shrink = \case
    Leaf -> []
    Node l a r -> concat
      [ [ Leaf ] -- The empty tree (optional, aggressive shrinking).
      , [ l, r ] -- The immediate subtrees.
      , [ Node l' a' r' | (l', a', r') <- shrink (l, a, r) ]
                 -- Shrinking in the subtrees.
      ]

instance CoArbitrary a => CoArbitrary (BST a) where
  coarbitrary = coarbitrary . toList

-- | Extensional equality.

(~~) :: Ord a => BST a -> BST a -> Bool
(~~) = (==) `on` toList

infix 1 ~~

-- | Check tree bounds.

bounded :: Ord a
  => Maybe a  -- ^ Lower bound (if any).
  -> BST a    -- ^ Tree to be checked.
  -> Maybe a  -- ^ Upper bound (if any).
  -> Bool     -- ^ Are all elements within bounds?
bounded low t high =
  case t of
    Leaf ->
      case (low, high) of
        (Just l, Just h) ->
          l <= h
        _ ->
          True
    Node l p r ->
      bounded low l (Just p) && bounded (Just p) r high

-- | Ordering invariant.

prop_bounded s = bounded Nothing s Nothing

-- Some properties of basic set operations

prop_member_insert x s = member x (insert x s)
prop_member_delete x s = not $ member x (delete x s)

-- Insertion and deletion are idempotent (also with each other)
prop_insert_insert x s = insert x (insert x s) ~~ insert x s
prop_insert_delete x s = insert x (delete x s) ~~ insert x s
prop_delete_insert x s = delete x (insert x s) ~~ delete x s
prop_delete_delete x s = delete x (delete x s) ~~ delete x s

-- properties of 'split'
prop_union_split x s = uncurry union (split x s) ~~ delete x s
prop_join_split  x s = uncurry join  (split x s) ~~ delete x s

-- 'union' makes a idempotent commutative monoid with the empty set as unit

prop_union_idem  s        = s `union` s ~~ s
prop_union_comm  s1 s2    = s1 `union` s2 ~~ s2 `union` s1
prop_union_assoc s1 s2 s3 = s1 `union` (s2 `union` s3) ~~ (s1 `union` s2) `union` s3
prop_union_unit_left  s   = empty `union` s ~~ s
prop_union_unit_right s   = s `union` empty ~~ s

-- 'fromList' is the inverse of 'toList'

prop_fromList_toList s    = fromList (toList s) ~~ s

-- Pseudo Template Haskell instruction to make $allProperties work
return []

tests :: TestTree
tests = testProperties "Tests" $allProperties

main :: IO ()
main = defaultMain tests
