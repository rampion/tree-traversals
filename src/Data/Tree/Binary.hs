{-# LANGUAGE DeriveFunctor #-}
module Data.Tree.Binary where

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint

-- | A binary tree
--
-- Since there are multiple ways to traverse a 'Tree', see 
-- "Data.Traversable.Tree.Binary" for newtype-wrappers with 'Traversable' instances.
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show, Functor, Eq)
