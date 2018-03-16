{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Different traversals for 'Data.BinaryTree.BinaryTree' from "Data.BinaryTree".
--
module Data.Traversable.BinaryTree
  ( inorder, InOrder(..)
  , preorder, PreOrder(..)
  , postorder, PostOrder(..)
  , levelorder, LevelOrder(..)
  , rlevelorder, RLevelOrder(..)
  ) where

import Data.Traversable (foldMapDefault)

import Control.Applicative.Batch
import Data.BinaryTree

-- $setup
-- >>> import PrettyPrinter.BinaryTree (prettyPrint)
-- >>> data Direction = L | R deriving Show
-- >>> :{
--     example :: BinaryTree [Direction]
--     example = 
--       Branch []
--       ( Branch [L]
--         ( Branch [L,L]
--           Leaf
--           ( Branch [L,L,R]
--             Leaf
--             Leaf
--           )
--         )
--         ( Branch [L,R]
--           ( Branch [L,R,L]
--             Leaf 
--             ( Branch [L,R,L,R]
--               Leaf
--               Leaf
--             )
--           )
--           Leaf
--         )
--       )
--       ( Branch [R]
--         Leaf
--         ( Branch [R,R]
--           ( Branch [R,R,L]
--             Leaf
--             Leaf
--           )
--           ( Branch [R,R,R]
--             Leaf
--             Leaf
--           )
--         )
--       )
--     :}


-- | 
-- Traverse the nodes of a tree from left-most to right-most.
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> _ <- inorder print example
-- [L,L]
-- [L,L,R]
-- [L]
-- [L,R,L]
-- [L,R,L,R]
-- [L,R]
-- []
-- [R]
-- [R,R,L]
-- [R,R]
-- [R,R,R]
inorder :: Applicative f => (a -> f b) -> BinaryTree a -> f (BinaryTree b)
inorder _ Leaf = pure Leaf
inorder f (Branch a la ra) = (\lb b rb -> Branch b lb rb) <$> inorder f la <*> f a <*> inorder f ra

-- | 
-- Traverse each node of the tree, then its left-subtree, then its right-subtree.
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> _ <- preorder print example
-- []
-- [L]
-- [L,L]
-- [L,L,R]
-- [L,R]
-- [L,R,L]
-- [L,R,L,R]
-- [R]
-- [R,R]
-- [R,R,L]
-- [R,R,R]
preorder :: Applicative f => (a -> f b) -> BinaryTree a -> f (BinaryTree b)
preorder _ Leaf = pure Leaf
preorder f (Branch a la ra) = Branch <$> f a <*> preorder f la <*> preorder f ra

-- | 
-- Traverse each node after traversing its left and right subtrees.
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> _ <- postorder print example
-- [L,L,R]
-- [L,L]
-- [L,R,L,R]
-- [L,R,L]
-- [L,R]
-- [L]
-- [R,R,L]
-- [R,R,R]
-- [R,R]
-- [R]
-- []
postorder :: Applicative f => (a -> f b) -> BinaryTree a -> f (BinaryTree b)
postorder _ Leaf = pure Leaf
postorder f (Branch a la ra) = (\lb rb b -> Branch b lb rb) <$> postorder f la <*> postorder f ra <*> f a

-- | 
-- Traverse each node of the tree in breadth-first order, left-to-right (i.e. all
-- nodes of depth zero, then all nodes of depth 1, then all nodes of depth 2,
-- etc.)
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> _ <- levelorder print example
-- []
-- [L]
-- [R]
-- [L,L]
-- [L,R]
-- [R,R]
-- [L,L,R]
-- [L,R,L]
-- [R,R,L]
-- [R,R,R]
-- [L,R,L,R]
levelorder :: Applicative f => (a -> f b) -> BinaryTree a -> f (BinaryTree b)
levelorder f = topDown $ \case
  Leaf           -> pure Leaf
  Branch a la ra -> Branch <$> lift (f a) <*> batch la <*> batch ra

-- | 
-- Traverse each node of the tree in breadth-last order, left-to-right (i.e. all
-- nodes of depth n, then all nodes of depth n-1, then all nodes of depth n-2,
-- etc.)
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> _ <- rlevelorder print example
-- [L,R,L,R]
-- [L,L,R]
-- [L,R,L]
-- [R,R,L]
-- [R,R,R]
-- [L,L]
-- [L,R]
-- [R,R]
-- [L]
-- [R]
-- []
rlevelorder :: Applicative f => (a -> f b) -> BinaryTree a -> f (BinaryTree b)
rlevelorder f = bottomUp $ \case
  Leaf           -> pure Leaf
  Branch a la ra -> Branch <$> lift (f a) <*> batch la <*> batch ra

-- | 'BinaryTree' wrapper to use 'inorder' traversal
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> mapM_ print $ InOrder example
-- [L,L]
-- [L,L,R]
-- [L]
-- [L,R,L]
-- [L,R,L,R]
-- [L,R]
-- []
-- [R]
-- [R,R,L]
-- [R,R]
-- [R,R,R]
newtype InOrder a = InOrder { getInOrder :: BinaryTree a }
  deriving Functor
instance Foldable InOrder where
  foldMap = foldMapDefault
instance Traversable InOrder where
  traverse f = fmap InOrder . inorder f . getInOrder

-- | 'BinaryTree' wrapper to use 'preorder' traversal
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> mapM_ print $ PreOrder example
-- []
-- [L]
-- [L,L]
-- [L,L,R]
-- [L,R]
-- [L,R,L]
-- [L,R,L,R]
-- [R]
-- [R,R]
-- [R,R,L]
-- [R,R,R]
newtype PreOrder a = PreOrder { getPreOrder :: BinaryTree a }
  deriving Functor
instance Foldable PreOrder where
  foldMap = foldMapDefault
instance Traversable PreOrder where
  traverse f = fmap PreOrder . preorder f . getPreOrder

-- | 'BinaryTree' wrapper to use 'postorder' traversal
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> mapM_ print $ PostOrder example
-- [L,L,R]
-- [L,L]
-- [L,R,L,R]
-- [L,R,L]
-- [L,R]
-- [L]
-- [R,R,L]
-- [R,R,R]
-- [R,R]
-- [R]
-- []
newtype PostOrder a = PostOrder { getPostOrder :: BinaryTree a }
  deriving Functor
instance Foldable PostOrder where
  foldMap = foldMapDefault
instance Traversable PostOrder where
  traverse f = fmap PostOrder . postorder f . getPostOrder

-- | 'BinaryTree' wrapper to use 'levelorder' traversal
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> mapM_ print $ LevelOrder example
-- []
-- [L]
-- [R]
-- [L,L]
-- [L,R]
-- [R,R]
-- [L,L,R]
-- [L,R,L]
-- [R,R,L]
-- [R,R,R]
-- [L,R,L,R]
newtype LevelOrder a = LevelOrder { getLevelOrder :: BinaryTree a }
  deriving Functor
instance Foldable LevelOrder where
  foldMap = foldMapDefault
instance Traversable LevelOrder where
  traverse f = fmap LevelOrder . levelorder f . getLevelOrder

-- | 'BinaryTree' wrapper to use 'rlevelorder' traversal
--
-- >>> prettyPrint example
--                                                []
--                     ┌──────────────────────────┴──────────────────────────┐
--                    [L]                                                   [R]
--     ┌───────────────┴───────────────┐                             ┌───────┴───────┐
--   [L,L]                           [L,R]                           ●             [R,R]
-- ┌───┴───┐                 ┌─────────┴─────────┐                               ┌───┴───┐
-- ●    [L,L,R]           [L,R,L]                ●                            [R,R,L] [R,R,R]
--        ┌┴┐           ┌────┴────┐                                             ┌┴┐     ┌┴┐
--        ● ●           ●     [L,R,L,R]                                         ● ●     ● ●
--                               ┌┴┐                                 
--                               ● ●                                 
-- >>> mapM_ print $ RLevelOrder example
-- [L,R,L,R]
-- [L,L,R]
-- [L,R,L]
-- [R,R,L]
-- [R,R,R]
-- [L,L]
-- [L,R]
-- [R,R]
-- [L]
-- [R]
-- []
newtype RLevelOrder a = RLevelOrder { getRLevelOrder :: BinaryTree a }
  deriving Functor
instance Foldable RLevelOrder where
  foldMap = foldMapDefault
instance Traversable RLevelOrder where
  traverse f = fmap RLevelOrder . rlevelorder f . getRLevelOrder
