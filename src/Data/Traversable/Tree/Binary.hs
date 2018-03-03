{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Traversable.Tree.Binary 
  ( inorder, InOrder(..)
  , preorder, PreOrder(..)
  , postorder, PostOrder(..)
  , depthorder, DepthOrder(..)
  ) where

import Data.Functor.Compose (Compose(..))
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
inorder :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
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
preorder :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
preorder _ Leaf = pure Leaf
preorder f (Branch a la ra) = (\b bl rb -> Branch b bl rb) <$> f a <*> preorder f la <*> preorder f ra

-- | 
-- Traverse each node of the tree, then its right-subtree, then its left-subtree.
--
-- >>> _ <- postorder print $ toDepth 3
-- []
-- [R]
-- [R,R]
-- [R,L]
-- [L]
-- [L,R]
-- [L,L]
postorder :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
postorder _ Leaf = pure Leaf
postorder f (Branch a la ra) = (\b rb bl -> Branch b bl rb) <$> f a <*> postorder f ra <*> postorder f la

-- | 
-- Traverse each node of the tree in depth-first order, left-to-right (i.e. all
-- nodes of depth zero, then all nodes of depth 1, then all nodes of depth 2,
-- etc.)
--
-- >>> _ <- depthorder print $ toDepth 3
-- []
-- [L]
-- [R]
-- [L,L]
-- [L,R]
-- [R,L]
-- [R,R]
depthorder :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
depthorder f = \ta -> schedule ta `evalPlan` byDepth where

  schedule :: Tree a -> Plan (Tree a) (Tree b) f (Tree b)
  schedule Leaf = pure Leaf
  schedule (Branch a la ra) = Branch <$> prepare (f a) <*> require la <*> require ra

  byDepth :: forall t. Traversable t => t (Tree a) -> f (t (Tree b))
  byDepth tta = if null tta -- need to check to prevent infinite recursion
    then -- tta is empty, so all this traversal does is alter the type and wrap it in f
         -- `pure (undefined <$> tta) works just as well
         fmap (fmap getDepthOrder . getCompose) . traverse f . Compose $ fmap DepthOrder tta 
    else traverse schedule tta `evalPlan` byDepth

{-
-- the bool specifies which child to prioritize
dfs, bfs :: forall f a b. Applicative f => (a -> (f b, Bool)) -> Tree a -> f (Tree b)

dfs _ Leaf = pure Leaf
dfs f (Branch a la ra) = case f a of
  (fb, L) -> (\b lb rb -> Branch b lb rb) <$> dfs f la <*> dfs f ra
  (fb, R)  -> (\b rb lb -> Branch b lb rb) <$> dfs f ra <*> dfs f la

bfs f = \ta -> schedule ta `evalPlan` byDepth where

  schedule :: Tree a -> Plan (Tree a) (Tree b) f (Tree b)
  schedule Leaf = pure Leaf
  schedule (Branch a la ra) = Branch <$> prepare (f a) <*> require la <*> require ra

  byDepth :: forall t. Traversable t => t (Tree a) -> f (t (Tree b))
  byDepth tta = if null tta -- need to check to prevent infinite recursion
    then -- tta is empty, so all this traversal does is alter the type and wrap it in f
         -- `pure (undefined <$> tta) works just as well
         fmap (fmap getDepthOrder . getCompose) . traverse f . Compose $ fmap DepthOrder tta 
    else traverse schedule tta `evalPlan` byDepth
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
-- []
-- [R]
-- [R,R]
-- [R,L]
-- [L]
-- [L,R]
-- [L,L]
-- >>> import Data.Foldable (toList)
-- >>> take 4 . toList $ PostOrder allDepths
-- [ [] , [ R ] , [ R , R ] , [ R , R , R ] ]
newtype PostOrder a = PostOrder { getPostOrder :: Tree a }
  deriving Functor
instance Foldable PostOrder where
  foldMap = foldMapDefault
instance Traversable PostOrder where
  traverse f = fmap PostOrder . postorder f . getPostOrder

-- | 'Tree' wrapper to use 'depthorder' traversal
--
-- >>> mapM_ print . DepthOrder $ toDepth 3
-- []
-- [L]
-- [R]
-- [L,L]
-- [L,R]
-- [R,L]
-- [R,R]
-- >>> import Data.Foldable (toList)
-- >>> take 4 . toList $ DepthOrder allDepths
-- [ [] , [ L ] , [ R ] , [ L , L ] ]
newtype DepthOrder a = DepthOrder { getDepthOrder :: Tree a }
  deriving Functor
instance Foldable DepthOrder where
  foldMap = foldMapDefault
instance Traversable DepthOrder where
  traverse f = fmap DepthOrder . depthorder f . getDepthOrder
