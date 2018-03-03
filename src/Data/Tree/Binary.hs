{-# LANGUAGE DeriveFunctor #-}
module Data.Tree.Binary where

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint

-- | A binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show, Functor, Eq)

-- data Direction = LeftBranch | RightBranch
--  - has Monoid instance preferring last
--  - useful for BFS/DFS

-- |
-- an infinite tree of all paths from the root ('False' for left, 'True' for right)
allDepths :: Tree [Bool]
allDepths = Branch [] (fmap (False:) allDepths) (fmap (True:) allDepths)

-- |
-- generate a finite version of 'allDepths' up to a certain depth
--
-- >>> toDepth 3
-- Branch
--   []
--   (Branch
--      [ False ]
--      (Branch [ False , False ] Leaf Leaf)
--      (Branch [ False , True ] Leaf Leaf))
--   (Branch
--      [ True ]
--      (Branch [ True , False ] Leaf Leaf)
--      (Branch [ True , True ] Leaf Leaf))
toDepth :: Int -> Tree [Bool]
toDepth 0 = Leaf
toDepth n = Branch [] (fmap (False:) t) (fmap (True:) t)
  where t = toDepth (n - 1)

