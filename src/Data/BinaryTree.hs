{-# LANGUAGE DeriveFunctor #-}
-- | A simple binary tree type, 'BinaryTree'.
module Data.BinaryTree 
  ( BinaryTree(..)
  ) where

-- | A binary tree
--
-- Since there are multiple ways to traverse a 'BinaryTree', see 
-- "Data.Traversable.TreeLike" for newtype-wrappers with 'Traversable' instances.
data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Show, Functor, Eq)
