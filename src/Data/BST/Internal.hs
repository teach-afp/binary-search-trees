
module Data.BST.Internal where

-- Setup for doctest:
-- The library code does not use 'toList', but the doctest code does.
-- We can add doctest imports in the special @$setup@ comment.

-- $setup
-- >>> import Data.Foldable (toList)

-- | Binary search trees: ordered, not necessarily balanced trees.
data BST a
    -- | Empty search tree.
  = Leaf
    -- | Binary node in search tree.
  | Node
      (BST a)  -- ^ Left subtree, with elements ≤ label.
      a        -- ^ Node label (pivot element).
      (BST a)  -- ^ Right subtree, with element ≥ label.
  deriving Show

---------------------------------------------------------------------------
-- API

-- | Empty binary search tree.
empty :: BST a
empty = Leaf

-- | Singleton binary search tree.
singleton :: a -> BST a
singleton a = Node Leaf a Leaf

-- | Insert into binary search tree.
--
-- Example:
--
-- >>> toList $ insert "baz" $ fromList ["bar","foo"]
-- ["bar","baz","foo"]
--
-- Nothing changes if element is already present:
--
-- >>> toList $ insert "baz" $ fromList ["bar","baz","foo"]
-- ["bar","baz","foo"]
--
insert :: Ord a
  => a       -- ^ Element to be inserted.
  -> BST a   -- ^ Tree to be inserted into.
  -> BST a   -- ^ Tree enriched with the inserted element.
insert a = go
  where
  go = \case
    Leaf ->
      singleton a
    Node l p r ->
      case compare a p of
        LT -> Node (go l) p r
        EQ -> Node l p r
        GT -> Node l p (go r)

-- | Create a binary search tree from a list.
fromList :: Ord a
  => [a]   -- ^ List to convert into tree.
  -> BST a -- ^ Tree, not containing duplicates.
fromList = foldr insert empty

-- | Query for membership.
--
-- Example (success):
--
-- >>> member "foo" $ fromList ["bar","baz","foo"]
-- True
--
-- Example (failure):
--
-- >>> member "fool" $ fromList ["bar","baz","foo"]
-- False
--
member :: Ord a
  => a      -- ^ Element to be tested for membership.
  -> BST a  -- ^ Tree to look for element.
  -> Bool   -- ^ Is element member of tree?
member a = go
  where
  go = \case
    Leaf ->
      False
    Node l p r ->
      case compare a p of
        LT -> go l
        EQ -> True
        GT -> go r

-- | Split a binary search tree along a new pivot element.
split :: Ord a
  => a              -- ^ Pivot element.
  -> BST a          -- ^ Tree to be split along pivot element.
  -> (BST a, BST a) -- ^ Left of pivot, right of pivot.
                    --   Both not containing pivot element.
split p = go
  where
  go = \case
    Leaf ->
      (Leaf, Leaf)
    Node l a r ->
      case compare p a of
        LT -> (l', Node r' a r) where (l', r') = go l
        EQ -> (l, r)
        GT -> (Node l a l', r') where (l', r') = go r

-- | Merge two binary search trees.
--
-- Example:
--
-- >>> toList $ union (fromList ["cat","dog"]) (fromList ["cat","tiger","wolf"])
-- ["cat","dog","tiger","wolf"]
--
union :: Ord a
  => BST a  -- ^ Tree to be merged in.
  -> BST a  -- ^ Tree to be merged into.
  -> BST a  -- ^ Tree containing the union of elements.
            --   Follows the structure of the second tree.
union t = \case
  Leaf ->
    t
  Node l p r ->
    Node (union l' l) p (union r' r) where (l', r') = split p t

-- | Delete from a binary search tree.
--
-- Example (element present):
--
-- >>> toList $ delete "foo" $ fromList ["bar","baz","foo"]
-- ["bar","baz"]
--
-- Example (element absent):
--
-- >>> toList $ delete "fool" $ fromList ["bar","baz","foo"]
-- ["bar","baz","foo"]
--
delete :: Ord a
  => a      -- ^ Element to be deleted.
  -> BST a  -- ^ Tree to delete from.
  -> BST a  -- ^ Tree containing everything but the deleted element.
delete a = go
  where
  go = \case
    Leaf ->
      Leaf
    Node l p r ->
      case compare a p of
        LT -> Node (go l) p r
        EQ -> join l r
        GT -> Node l p (go r)

---------------------------------------------------------------------------
-- Internal functions

-- | Merge two binary search trees where each element of the first tree
--   is smaller or equal each element of the second tree.
--
--   @'join' t1 t2 == 'union' t1 t2@ but implemented without comparisons.
join :: Ord a
  => BST a
  -> BST a
  -> BST a
join = curry $ \case
  (Leaf, t2) ->
    t2
  (t1, Leaf) ->
    t1
  (Node l1 p1 r1, Node l2 p2 r2) ->
    Node l1 p1 (Node (join r1 l2) p2 r2)

---------------------------------------------------------------------------
-- Instances

instance Ord a => Semigroup (BST a) where
  (<>) = union

instance Ord a => Monoid (BST a) where
  mempty = empty

instance Foldable BST where
  foldMap :: Monoid m => (a -> m) -> BST a -> m
  foldMap f = go
    where
    go = \case
      Leaf ->
        mempty
      Node l p r ->
        go l <> f p <> go r
