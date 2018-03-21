{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Traversable.TreeLike where

import Control.Applicative ((<**>))
import Data.Functor.Compose (Compose(..))
import Data.Traversable (foldMapDefault)
import Data.Tree

import Data.BinaryTree

class Functor tree => TreeLike tree where
  treeTraverse :: Applicative f 
               => (a -> f b)
               -> (forall subtree. TreeLike subtree => subtree a -> f (subtree b))
               -> tree a -> f (tree b)

instance TreeLike Tree where
  treeTraverse f g (Node a as) = Node <$> f a <*> traverse g as

instance TreeLike BinaryTree where
  treeTraverse _ _ Leaf = pure Leaf
  treeTraverse f g (Branch a l r) = flip Branch <$> g l <*> f a <*> g r

instance (Traversable f, TreeLike tree) => TreeLike (Compose f tree) where
  treeTraverse _ g (Compose trees) = Compose <$> traverse g trees
  
inorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
inorder f = treeTraverse f (inorder f)

data Batch query response f a = forall trees. Batch
  { lifted   :: f (TreeList trees response -> a)
  , batched  :: TreeList trees query
  }

instance Functor f => Functor (Batch query response f) where
  fmap f (Batch lifted batched) = Batch
    { lifted = (f .) <$> lifted
    , batched = batched
    }

instance Applicative f => Applicative (Batch query response f) where
  pure = lift . pure
  Batch lf bf <*> Batch la ba = append bf ba $ \split -> Batch
    $ (\kf ka (split -> ~(bf,ba)) -> kf bf (ka ba)) <$> lf <*> la

append :: TreeList x a 
       -> TreeList y a 
       -> (forall z. (forall b. TreeList z b -> (TreeList x b, TreeList y b)) -> TreeList z a -> r)
       -> r
append TreeNil trees k = k (\trees -> (TreeNil, trees)) trees
append (TreeCons t x) y k = append x y $ \split -> 
  k (\(TreeCons t (split -> ~(x, y))) -> (TreeCons t x, y)) . TreeCons t

data TreeList trees a where
  TreeNil :: TreeList '[] a
  TreeCons :: TreeLike tree => tree a -> TreeList trees a -> TreeList (tree ': trees) a

treeListTraverse :: Applicative f => (forall tree. TreeLike tree => tree a -> f (tree b)) -> TreeList trees a -> f (TreeList trees b)
treeListTraverse _ TreeNil = pure TreeNil
treeListTraverse f (TreeCons tree trees) = TreeCons <$> f tree <*> treeListTraverse f trees

lift :: Functor f => f a -> Batch query response f a
lift fa = Batch
  { lifted = const <$> fa
  , batched = TreeNil
  }

batch :: Applicative f => TreeLike tree => tree query -> Batch query response f (tree response)
batch tree = Batch
  { lifted = pure $ \(TreeCons tree TreeNil) -> tree
  , batched = TreeCons tree TreeNil
  }

batchSubTrees :: (Applicative f, TreeLike tree)
              => (a -> f b) -> tree a -> Batch a b f (tree b)
batchSubTrees f = treeTraverse (lift . f) batch 

runBatchWith :: Batch query response f a -> (forall trees. f (TreeList trees response -> a) -> TreeList trees query -> f a) -> f a
runBatchWith (Batch lifted batched) k = k lifted batched

preorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
preorder f tree = batchSubTrees f tree `runBatchWith` \nb ta ->
  nb <*> treeListTraverse (preorder f) ta
  
postorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
postorder f tree = batchSubTrees f tree `runBatchWith` \nb ta ->
  treeListTraverse (postorder f) ta <**> nb

levelorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
levelorder = \f tree -> batchSubTrees f tree `runBatchWith` topDownHandler f where

  topDownHandler :: Applicative f
                 => (a -> f b) 
                 -> f (TreeList trees b -> r)
                 -> TreeList trees a
                 -> f r
  topDownHandler f nb ta = nb <*> case treeListTraverse (const Nothing) ta of
    Just tb -> pure tb -- avoid infinite recursion on empty traversables
    Nothing -> treeListTraverse (batchSubTrees f) ta `runBatchWith` topDownHandler f

rlevelorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
rlevelorder = \f tree -> batchSubTrees f tree `runBatchWith` bottomUpHandler f where

  bottomUpHandler :: Applicative f
                  => (a -> f b) 
                  -> f (TreeList trees b -> r)
                  -> TreeList trees a
                  -> f r
  bottomUpHandler f nb ta = (<**> nb) $ case treeListTraverse (const Nothing) ta of
    Just tb -> pure tb -- avoid infinite recursion on empty traversables
    Nothing -> treeListTraverse (batchSubTrees f) ta `runBatchWith` bottomUpHandler f

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
