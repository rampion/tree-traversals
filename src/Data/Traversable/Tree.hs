{-# LANGUAGE DeriveFunctor #-}
-- |
-- Different traversals for 'Data.Tree.Tree' from "Data.Tree".
--
module Data.Traversable.Tree
  ( preorder, PreOrder(..)
  , postorder, PostOrder(..)
  , levelorder, LevelOrder(..)
  , rlevelorder, RLevelOrder(..)
  )
  where

import Data.Traversable (foldMapDefault)
import Data.Tree

import Control.Applicative.Batch

-- $setup
-- >>> import Data.Tree.PrettyPrinter (prettyPrint)
-- >>> :{
--     example :: Tree [Int]
--     example = Node []
--       [ Node [0] []
--       , Node [1]
--         [ Node [1,0] [] ]
--       , Node [2]
--         [ Node [2,0] []
--         , Node [2,1]
--           [ Node [2,1,0] [] ]
--         ]
--       , Node [3]
--         [ Node [3,0] []
--         , Node [3,1]
--           [ Node [3,1,0] [] ]
--         , Node [3,2]
--           [ Node [3,2,0] []
--           , Node [3,2,1] 
--             [ Node [3,2,1,0] [] ]
--           ]
--         ]
--       ]
--     :}

-- |
-- Traverse each node of the tree, then its subtrees.
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
-- >>> _ <- preorder print example
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
preorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
preorder f (Node a ts) = Node <$> f a <*> traverse (preorder f) ts

-- |
-- Traverse each node after traversing its subtrees.
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
-- >>> _ <- postorder print example
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
postorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
postorder f (Node a ts) = flip Node <$> traverse (postorder f) ts <*> f a

-- |
-- Traverse each node of the tree in breadth-first order, left-to-right (i.e. all
-- nodes of depth zero, then all nodes of depth 1, then all nodes of depth 2,
-- etc.)
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
-- >>> _ <- levelorder print example
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
levelorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
levelorder f = topDown $ \(Node a ts) -> Node <$> lift (f a) <*> traverse batch ts

-- |
-- Traverse each node of the tree in breadth-last order, left-to-right (i.e. all
-- nodes of depth n, then all nodes of depth n-1, then all nodes of depth n-2,
-- etc.)
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
-- >>> _ <- rlevelorder print example
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
rlevelorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
rlevelorder f = bottomUp $ \(Node a ts) -> Node <$> lift (f a) <*> traverse batch ts

-- TODO : Forests?

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
newtype PreOrder a = PreOrder { getPreOrder :: Tree a }
  deriving Functor
instance Foldable PreOrder where
  foldMap = foldMapDefault
instance Traversable PreOrder where
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
newtype PostOrder a = PostOrder { getPostOrder :: Tree a }
  deriving Functor
instance Foldable PostOrder where
  foldMap = foldMapDefault
instance Traversable PostOrder where
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
newtype LevelOrder a = LevelOrder { getLevelOrder :: Tree a }
  deriving Functor
instance Foldable LevelOrder where
  foldMap = foldMapDefault
instance Traversable LevelOrder where
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
newtype RLevelOrder a = RLevelOrder { getRLevelOrder :: Tree a }
  deriving Functor
instance Foldable RLevelOrder where
  foldMap = foldMapDefault
instance Traversable RLevelOrder where
  traverse f = fmap RLevelOrder . rlevelorder f . getRLevelOrder
