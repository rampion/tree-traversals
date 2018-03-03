{-# LANGUAGE DeriveFunctor #-}
module Data.Tree.Binary where

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint

-- | A binary tree
--
-- Since there are multiple ways to traverse a 'Tree', see 
-- 'Data.Traversable.Tree.Binary' for newtype-wrappers with 'Traversable' instances.
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show, Functor, Eq)

data Direction = L | R
  deriving (Show, Eq, Enum)

type Path = [Direction]

{-
-- used to make 'Data.Traversable.Tree.Binary.bfs' and 'Data.Traversable.Tree.Binary.dfs'
-- legal Traversals.
instance Monoid Direction where
  _ `mappend` dir = dir
  mempty = ?
-}

-- |
-- an infinite tree of all paths from the root
allDepths :: Tree Path
allDepths = Branch [] (fmap (L:) allDepths) (fmap (R:) allDepths)

-- |
-- generate a finite version of 'allDepths' up to a certain depth
--
-- >>> toDepth 3
-- Branch
--   []
--   (Branch
--      [ L ] (Branch [ L , L ] Leaf Leaf) (Branch [ L , R ] Leaf Leaf))
--   (Branch
--      [ R ] (Branch [ R , L ] Leaf Leaf) (Branch [ R , R ] Leaf Leaf))
toDepth :: Int -> Tree Path
toDepth 0 = Leaf
toDepth n = Branch [] (fmap (L:) t) (fmap (R:) t)
  where t = toDepth (n - 1)

