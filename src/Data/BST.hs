-- | Sets implemented as binary search trees, not necessarily balanced.

module Data.BST (module Exports) where

import Data.BST.Internal as Exports
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
