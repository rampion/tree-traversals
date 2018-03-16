{-# LANGUAGE DeriveFunctor #-}
-- | A simple binary tree type, 'Tree'.
module Data.Tree.Binary 
  ( Tree(..)
  ) where

-- | A binary tree
--
-- Since there are multiple ways to traverse a 'Tree', see 
-- "Data.Traversable.Tree.Binary" for newtype-wrappers with 'Traversable' instances.
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show, Functor, Eq)
