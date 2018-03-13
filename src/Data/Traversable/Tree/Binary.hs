{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Traversable.Tree.Binary 
  ( inorder, InOrder(..)
  , preorder, PreOrder(..)
  , postorder, PostOrder(..)
  , levelorder, LevelOrder(..)
  ) where

import Control.Applicative.Backwards (Backwards(..))
-- import Data.Functor.Compose (Compose(..))
import Data.Traversable (foldMapDefault)

import Control.Applicative.Trans.Plan
import Data.Tree.Binary

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint

-- | 
-- Traverse the nodes of a tree from left-most to right-most.
--
-- >>> _ <- inorder print $ toDepth 3
-- [L,L]
-- [L]
-- [L,R]
-- []
-- [R,L]
-- [R]
-- [R,R]
inorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
inorder _ Leaf = pure Leaf
inorder f (Branch a la ra) = (\lb b rb -> Branch b lb rb) <$> inorder f la <*> f a <*> inorder f ra

-- | 
-- Traverse each node of the tree, then its left-subtree, then its right-subtree.
--
-- >>> _ <- preorder print $ toDepth 3
-- []
-- [L]
-- [L,L]
-- [L,R]
-- [R]
-- [R,L]
-- [R,R]
preorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
preorder _ Leaf = pure Leaf
preorder f (Branch a la ra) = Branch <$> f a <*> preorder f la <*> preorder f ra

-- | 
-- Traverse each node after traversing its left and right subtrees.
--
-- >>> _ <- postorder print $ toDepth 3
-- [L,L]
-- [L,R]
-- [L]
-- [R,L]
-- [R,R]
-- [R]
-- []
postorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
postorder _ Leaf = pure Leaf
postorder f (Branch a la ra) = (\lb rb b -> Branch b lb rb) <$> postorder f la <*> postorder f ra <*> f a

-- | 
-- Traverse each node of the tree in breadth-first order, left-to-right (i.e. all
-- nodes of depth zero, then all nodes of depth 1, then all nodes of depth 2,
-- etc.)
--
-- >>> _ <- levelorder print $ toDepth 3
-- []
-- [L]
-- [R]
-- [L,L]
-- [L,R]
-- [R,L]
-- [R,R]
levelorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
levelorder f = runWithQueue $ \case
  Leaf           -> pure Leaf
  Branch a la ra -> Branch <$> prepare (f a) <*> require la <*> require ra

-- | 
-- Traverse each node of the tree in breadth-last order, left-to-right (i.e. all
-- nodes of depth n, then all nodes of depth n-1, then all nodes of depth n-2,
-- etc.)
--
-- >>> _ <- rlevelorder print $ toDepth 3
-- [L,L]
-- [L,R]
-- [R,L]
-- [R,R]
-- [L]
-- [R]
-- []
rlevelorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
rlevelorder f = (forwards .) . runWithQueue $ \case
  Leaf           -> pure Leaf
  Branch a la ra -> (\b rb lb -> Branch b lb rb) <$> prepare (Backwards $ f a) <*> require ra <*> require la

{-
type Search = Compose ((,) (Last Direction))

dfs :: Applicative f => (a -> Search f b) -> Tree a -> Search f (Tree b)
dfs _ Leaf = pure Leaf
dfs f (Branch a la ra) = let fb@(Compose (Last d, _)) = f a in case d of
  L -> Branch <$> fb <*> dfs f la <*> dfs f ra
  R -> (flip . Branch) <$> fb <*> dfs f ra <*> dfs f la

bfs :: Applicative f => (a -> Search f b) -> Tree a -> Search f (Tree b)
bfs f = runWithQueue $ \case
  Leaf -> pure Leaf
  Branch a la ra -> let fb@(Compose (Last d, _)) = f a in case d of
    L -> Branch <$> prepare fb <*> require la <*> require ra
    R -> (flip . Branch) <$> prepare fb <*> require la <*> require ra
    -}
    
-- | 'Tree' wrapper to use 'inorder' traversal
--
-- >>> mapM_ print . InOrder $ toDepth 3
-- [L,L]
-- [L]
-- [L,R]
-- []
-- [R,L]
-- [R]
-- [R,R]
newtype InOrder a = InOrder { getInOrder :: Tree a }
  deriving Functor
instance Foldable InOrder where
  foldMap = foldMapDefault
instance Traversable InOrder where
  traverse f = fmap InOrder . inorder f . getInOrder

-- | 'Tree' wrapper to use 'preorder' traversal
--
-- >>> mapM_ print . PreOrder $ toDepth 3
-- []
-- [L]
-- [L,L]
-- [L,R]
-- [R]
-- [R,L]
-- [R,R]
-- >>> import Data.Foldable (toList)
-- >>> take 4 . toList $ PreOrder allDepths
-- [ [] , [ L ] , [ L , L ] , [ L , L , L ] ]
newtype PreOrder a = PreOrder { getPreOrder :: Tree a }
  deriving Functor
instance Foldable PreOrder where
  foldMap = foldMapDefault
instance Traversable PreOrder where
  traverse f = fmap PreOrder . preorder f . getPreOrder

-- | 'Tree' wrapper to use 'postorder' traversal
--
-- >>> mapM_ print . PostOrder $ toDepth 3
-- [L,L]
-- [L,R]
-- [L]
-- [R,L]
-- [R,R]
-- [R]
-- []
newtype PostOrder a = PostOrder { getPostOrder :: Tree a }
  deriving Functor
instance Foldable PostOrder where
  foldMap = foldMapDefault
instance Traversable PostOrder where
  traverse f = fmap PostOrder . postorder f . getPostOrder

-- | 'Tree' wrapper to use 'levelorder' traversal
--
-- >>> mapM_ print . LevelOrder $ toDepth 3
-- []
-- [L]
-- [R]
-- [L,L]
-- [L,R]
-- [R,L]
-- [R,R]
-- >>> import Data.Foldable (toList)
-- >>> take 4 . toList $ LevelOrder allDepths
-- [ [] , [ L ] , [ R ] , [ L , L ] ]
newtype LevelOrder a = LevelOrder { getLevelOrder :: Tree a }
  deriving Functor
instance Foldable LevelOrder where
  foldMap = foldMapDefault
instance Traversable LevelOrder where
  traverse f = fmap LevelOrder . levelorder f . getLevelOrder

-- | 'Tree' wrapper to use 'rlevelorder' traversal
--
-- >>> mapM_ print . RLevelOrder $ toDepth 3
-- [L,L]
-- [L,R]
-- [R,L]
-- [R,R]
-- [L]
-- [R]
-- []
newtype RLevelOrder a = RLevelOrder { getRLevelOrder :: Tree a }
  deriving Functor
instance Foldable RLevelOrder where
  foldMap = foldMapDefault
instance Traversable RLevelOrder where
  traverse f = fmap RLevelOrder . rlevelorder f . getRLevelOrder
