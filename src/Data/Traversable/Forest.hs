{-# LANGUAGE DeriveFunctor #-}
-- |
-- Different traversals for 'Data.Tree.Forest' from "Data.Tree".
--
module Data.Traversable.Forest
  ( preorder, PreOrder(..)
  , postorder, PostOrder(..)
  , levelorder, LevelOrder(..)
  , rlevelorder, RLevelOrder(..)
  ) where

import Data.Traversable (foldMapDefault)
import Data.Tree

import Control.Applicative.Batch
import qualified Data.Traversable.Tree as Tree

-- $setup
-- >>> import PrettyPrinter.Forest (prettyPrint)
-- >>> :{
--     example :: Forest [Int]
--     example =
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
-- Traverse each 'Data.Tree.Tree' in the 'Data.Tree.Forest' in
-- 'Data.Traversable.Tree.preorder' before traversing the next one.
--
-- >>> prettyPrint example
--  ┌────┬────────┬───────────────────┐
-- [0]  [1]      [2]                 [3]
--       │     ┌──┴───┐      ┌──────┬─┴─────────┐
--     [1,0] [2,0]  [2,1]  [3,0]  [3,1]       [3,2]
--                    │             │       ┌───┴────┐
--                 [2,1,0]       [3,1,0] [3,2,0]  [3,2,1]
--                                                   │
--                                               [3,2,1,0]
-- >>> _ <- preorder print example
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
preorder :: Applicative f => (a -> f b) -> Forest a -> f (Forest b)
preorder = traverse . Tree.preorder

-- |
-- Traverse each 'Data.Tree.Tree' in the 'Data.Tree.Forest' in
-- 'Data.Traversable.Tree.postorder' before traversing the next one.
--
-- >>> prettyPrint example
--  ┌────┬────────┬───────────────────┐
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
postorder :: Applicative f => (a -> f b) -> Forest a -> f (Forest b)
postorder = traverse . Tree.postorder

-- |
-- Traverse each node of the 'Data.Tree.Forest' in breadth-first order,
-- left-to-right (i.e. all nodes of depth zero, then all nodes of depth 1, then
-- all nodes of depth 2, etc.)
--
-- >>> prettyPrint example
--  ┌────┬────────┬───────────────────┐
-- [0]  [1]      [2]                 [3]
--       │     ┌──┴───┐      ┌──────┬─┴─────────┐
--     [1,0] [2,0]  [2,1]  [3,0]  [3,1]       [3,2]
--                    │             │       ┌───┴────┐
--                 [2,1,0]       [3,1,0] [3,2,0]  [3,2,1]
--                                                   │
--                                               [3,2,1,0]
-- >>> _ <- levelorder print example
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
levelorder :: Applicative f => (a -> f b) -> Forest a -> f (Forest b)
levelorder f = topDownAll $ \(Node a ts) ->  Node <$> lift (f a) <*> traverse batch ts

-- |
-- Traverse each node of the 'Data.Tree.Forest' in breadth-last order,
-- left-to-right (i.e. all nodes of depth n, then all nodes of depth n-1, then
-- all nodes of depth n-2, etc.)
--
-- >>> prettyPrint example
--  ┌────┬────────┬───────────────────┐
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
rlevelorder :: Applicative f => (a -> f b) -> Forest a -> f (Forest b)
rlevelorder f = bottomUpAll $ \(Node a ts) ->  Node <$> lift (f a) <*> traverse batch ts

-- | 'Forest' wrapper to use 'preorder' traversal
--
-- >>> prettyPrint example
--  ┌────┬────────┬───────────────────┐
-- [0]  [1]      [2]                 [3]
--       │     ┌──┴───┐      ┌──────┬─┴─────────┐
--     [1,0] [2,0]  [2,1]  [3,0]  [3,1]       [3,2]
--                    │             │       ┌───┴────┐
--                 [2,1,0]       [3,1,0] [3,2,0]  [3,2,1]
--                                                   │
--                                               [3,2,1,0]
-- >>> mapM_ print $ PreOrder example
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
newtype PreOrder a = PreOrder { getPreOrder :: Forest a }
  deriving Functor
instance Foldable PreOrder where
  foldMap = foldMapDefault
instance Traversable PreOrder where
  traverse f = fmap PreOrder . preorder f . getPreOrder

-- | 'Forest' wrapper to use 'postorder' traversal
--
-- >>> prettyPrint example
--  ┌────┬────────┬───────────────────┐
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
newtype PostOrder a = PostOrder { getPostOrder :: Forest a }
  deriving Functor
instance Foldable PostOrder where
  foldMap = foldMapDefault
instance Traversable PostOrder where
  traverse f = fmap PostOrder . postorder f . getPostOrder

-- | 'Forest' wrapper to use 'levelorder' traversal
--
-- >>> prettyPrint example
--  ┌────┬────────┬───────────────────┐
-- [0]  [1]      [2]                 [3]
--       │     ┌──┴───┐      ┌──────┬─┴─────────┐
--     [1,0] [2,0]  [2,1]  [3,0]  [3,1]       [3,2]
--                    │             │       ┌───┴────┐
--                 [2,1,0]       [3,1,0] [3,2,0]  [3,2,1]
--                                                   │
--                                               [3,2,1,0]
-- >>> mapM_ print $ LevelOrder example
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
newtype LevelOrder a = LevelOrder { getLevelOrder :: Forest a }
  deriving Functor
instance Foldable LevelOrder where
  foldMap = foldMapDefault
instance Traversable LevelOrder where
  traverse f = fmap LevelOrder . levelorder f . getLevelOrder

-- | 'Forest' wrapper to use 'rlevelorder' traversal
--
-- >>> prettyPrint example
--  ┌────┬────────┬───────────────────┐
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
newtype RLevelOrder a = RLevelOrder { getRLevelOrder :: Forest a }
  deriving Functor
instance Foldable RLevelOrder where
  foldMap = foldMapDefault
instance Traversable RLevelOrder where
  traverse f = fmap RLevelOrder . rlevelorder f . getRLevelOrder
