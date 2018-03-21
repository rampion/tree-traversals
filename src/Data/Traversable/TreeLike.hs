{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.Traversable.TreeLike where

import Control.Applicative ((<**>))
import Data.Functor.Compose (Compose(..))
import Data.Traversable (foldMapDefault)
import Data.Tree

import Control.Applicative.Batch
import Data.BinaryTree

class (Functor tree, TreeLike (Sub tree)) => TreeLike tree where
  type Sub tree :: * -> *
  treeTraverse :: Applicative f => (a -> f b) -> (Sub tree a -> f (Sub tree b)) -> tree a -> f (tree b)

instance TreeLike Tree where
  type Sub Tree = Tree
  treeTraverse f g (Node a as) = Node <$> f a <*> traverse g as

instance TreeLike BinaryTree where
  type Sub BinaryTree = BinaryTree
  treeTraverse _ _ Leaf = pure Leaf
  treeTraverse f g (Branch a l r) = flip Branch <$> g l <*> f a <*> g r

instance (Traversable f, TreeLike tree) => TreeLike (Compose f tree) where
  type Sub (Compose f tree) = tree
  treeTraverse _ g (Compose trees) = Compose <$> traverse g trees
  
inorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
inorder f = treeTraverse f (inorder f)
  
batchSubTrees :: (Applicative f, TreeLike tree)
              => (a -> f b) -> tree a -> Batch (Sub tree a) (Sub tree b) f (tree b)
batchSubTrees f = treeTraverse (lift . f) batch 

preorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
preorder f tree = batchSubTrees f tree `runBatchWith` \nb ta ->
  nb <*> traverse (preorder f) ta
  
postorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
postorder f tree = batchSubTrees f tree `runBatchWith` \nb ta ->
  traverse (postorder f) ta <**> nb

levelorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
levelorder = \f tree -> batchSubTrees f tree `runBatchWith` topDownHandler f where

  topDownHandler :: (Applicative f, Traversable t, TreeLike tree)
                 => (a -> f b) 
                 -> f (t (tree b) -> r)
                 -> t (tree a)
                 -> f r
  topDownHandler f nb ta = nb <*> case traverse (const Nothing) ta of
    Just tb -> pure tb -- avoid infinite recursion on empty traversables
    Nothing -> traverse (batchSubTrees f) ta `runBatchWith` topDownHandler f

rlevelorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
rlevelorder = \f tree -> batchSubTrees f tree `runBatchWith` bottomUpHandler f where

  bottomUpHandler :: (Applicative f, Traversable t, TreeLike tree)
                  => (a -> f b) 
                  -> f (t (tree b) -> r)
                  -> t (tree a)
                  -> f r
  bottomUpHandler f nb ta = (<**> nb) $ case traverse (const Nothing) ta of
    Just tb -> pure tb -- avoid infinite recursion on empty traversables
    Nothing -> traverse (batchSubTrees f) ta `runBatchWith` bottomUpHandler f

newtype InOrder tree a = InOrder { getInOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (InOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (InOrder tree) where
  traverse f = fmap InOrder . inorder f . getInOrder

-- | 'Tree' wrapper to use 'preorder' traversal
--
-- >>> prettyPrint example
--                   []
--  ┌────┬────────┬──┴────────────────┐
-- [0]  [1]      [2]                 [3]
--       │     ┌──┴───┐      ┌──────┬─┴─────────┐
--     [1,0] [2,0]  [2,1]  [3,0]  [3,1]       [3,2]
--                    │             │       ┌───┴────┐
--                 [2,1,0]       [3,1,0] [3,2,0]  [3,2,1]
--                                                   │
--                                               [3,2,1,0]
-- >>> mapM_ print $ PreOrder example
-- []
-- [0]
-- [1]
-- [1,0]
-- [2]
-- [2,0]
-- [2,1]
-- [2,1,0]
-- [3]
-- [3,0]
-- [3,1]
-- [3,1,0]
-- [3,2]
-- [3,2,0]
-- [3,2,1]
-- [3,2,1,0]
newtype PreOrder tree a = PreOrder { getPreOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (PreOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (PreOrder tree) where
  traverse f = fmap PreOrder . preorder f . getPreOrder

-- | 'Tree' wrapper to use 'postorder' traversal
--
-- >>> prettyPrint example
--                   []
--  ┌────┬────────┬──┴────────────────┐
-- [0]  [1]      [2]                 [3]
--       │     ┌──┴───┐      ┌──────┬─┴─────────┐
--     [1,0] [2,0]  [2,1]  [3,0]  [3,1]       [3,2]
--                    │             │       ┌───┴────┐
--                 [2,1,0]       [3,1,0] [3,2,0]  [3,2,1]
--                                                   │
--                                               [3,2,1,0]
-- >>> mapM_ print $ PostOrder example
-- [0]
-- [1,0]
-- [1]
-- [2,0]
-- [2,1,0]
-- [2,1]
-- [2]
-- [3,0]
-- [3,1,0]
-- [3,1]
-- [3,2,0]
-- [3,2,1,0]
-- [3,2,1]
-- [3,2]
-- [3]
-- []
newtype PostOrder tree a = PostOrder { getPostOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (PostOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (PostOrder tree) where
  traverse f = fmap PostOrder . postorder f . getPostOrder

-- | 'Tree' wrapper to use 'levelorder' traversal
--
-- >>> prettyPrint example
--                   []
--  ┌────┬────────┬──┴────────────────┐
-- [0]  [1]      [2]                 [3]
--       │     ┌──┴───┐      ┌──────┬─┴─────────┐
--     [1,0] [2,0]  [2,1]  [3,0]  [3,1]       [3,2]
--                    │             │       ┌───┴────┐
--                 [2,1,0]       [3,1,0] [3,2,0]  [3,2,1]
--                                                   │
--                                               [3,2,1,0]
-- >>> mapM_ print $ LevelOrder example
-- []
-- [0]
-- [1]
-- [2]
-- [3]
-- [1,0]
-- [2,0]
-- [2,1]
-- [3,0]
-- [3,1]
-- [3,2]
-- [2,1,0]
-- [3,1,0]
-- [3,2,0]
-- [3,2,1]
-- [3,2,1,0]
newtype LevelOrder tree a = LevelOrder { getLevelOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (LevelOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (LevelOrder tree) where
  traverse f = fmap LevelOrder . levelorder f . getLevelOrder

-- | 'Tree' wrapper to use 'rlevelorder' traversal
--
-- >>> prettyPrint example
--                   []
--  ┌────┬────────┬──┴────────────────┐
-- [0]  [1]      [2]                 [3]
--       │     ┌──┴───┐      ┌──────┬─┴─────────┐
--     [1,0] [2,0]  [2,1]  [3,0]  [3,1]       [3,2]
--                    │             │       ┌───┴────┐
--                 [2,1,0]       [3,1,0] [3,2,0]  [3,2,1]
--                                                   │
--                                               [3,2,1,0]
-- >>> mapM_ print $ RLevelOrder example
-- [3,2,1,0]
-- [2,1,0]
-- [3,1,0]
-- [3,2,0]
-- [3,2,1]
-- [1,0]
-- [2,0]
-- [2,1]
-- [3,0]
-- [3,1]
-- [3,2]
-- [0]
-- [1]
-- [2]
-- [3]
-- []
newtype RLevelOrder tree a = RLevelOrder { getRLevelOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (RLevelOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (RLevelOrder tree) where
  traverse f = fmap RLevelOrder . rlevelorder f . getRLevelOrder
