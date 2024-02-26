-- | Sets implemented as binary search trees, not necessarily balanced.

module Data.BST
  ( BST
  -- * Construction
  , empty
  , singleton
  , insert
  , fromList
  , union
  -- * Deletion
  , delete
  , split
  -- * Query
  , member
  )
where

import Data.BST.Internal
