module Data.BST.Internal where

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

-- | Empty binary search tree.
empty :: BST a
empty = Leaf

-- | Singleton binary search tree.
singleton :: a -> BST a
singleton a = Node Leaf a Leaf

-- | Insert into binary search tree.
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
        EQ -> union l r
        GT -> Node l p (go r)

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
