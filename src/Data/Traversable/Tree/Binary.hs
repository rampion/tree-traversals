{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Traversable.Tree.Binary where

import Data.Functor.Compose (Compose(..))
import Data.Traversable (foldMapDefault)

import Control.Applicative.Trans.Plan
import Data.Tree.Binary

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint

-- | 
-- various traversals of a tree
-- 
-- >>> _ <- inorder print $ toDepth 3
-- [False,False]
-- [False]
-- [False,True]
-- []
-- [True,False]
-- [True]
-- [True,True]
-- >>> _ <- preorder print $ toDepth 3
-- []
-- [False]
-- [False,False]
-- [False,True]
-- [True]
-- [True,False]
-- [True,True]
-- >>> _ <- postorder print $ toDepth 3
-- []
-- [True]
-- [True,True]
-- [True,False]
-- [False]
-- [False,True]
-- [False,False]
-- >>> _ <- levelorder print $ toDepth 3
-- []
-- [False]
-- [True]
-- [False,False]
-- [False,True]
-- [True,False]
-- [True,True]
inorder, preorder, postorder, levelorder
  :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)

inorder _ Leaf = pure Leaf
inorder f (Branch a la ra) = (\lb b rb -> Branch b lb rb) <$> inorder f la <*> f a <*> inorder f ra

preorder _ Leaf = pure Leaf
preorder f (Branch a la ra) = (\b bl rb -> Branch b bl rb) <$> f a <*> preorder f la <*> preorder f ra

postorder _ Leaf = pure Leaf
postorder f (Branch a la ra) = (\b rb bl -> Branch b bl rb) <$> f a <*> postorder f ra <*> postorder f la

levelorder f = \ta -> schedule ta `evalPlan` byLevel where

  schedule :: Tree a -> Plan (Tree a) (Tree b) f (Tree b)
  schedule Leaf = pure Leaf
  schedule (Branch a la ra) = Branch <$> prepare (f a) <*> require la <*> require ra

  byLevel :: forall t. Traversable t => t (Tree a) -> f (t (Tree b))
  byLevel tta = if null tta -- need to check to prevent infinite recursion
    then -- tta is empty, so all this traversal does is alter the type and wrap it in f
         -- `pure (undefined <$> tta) works just as well
         fmap (fmap getLevelOrder . getCompose) . traverse f . Compose $ fmap LevelOrder tta 
    else traverse schedule tta `evalPlan` byLevel

{-
-- the bool specifies which child to prioritize
dfs, bfs :: forall f a b. Applicative f => (a -> (f b, Bool)) -> Tree a -> f (Tree b)

dfs _ Leaf = pure Leaf
dfs f (Branch a la ra) = case f a of
  (fb, False) -> (\b lb rb -> Branch b lb rb) <$> dfs f la <*> dfs f ra
  (fb, True)  -> (\b rb lb -> Branch b lb rb) <$> dfs f ra <*> dfs f la

bfs f = \ta -> schedule ta `evalPlan` byLevel where

  schedule :: Tree a -> Plan (Tree a) (Tree b) f (Tree b)
  schedule Leaf = pure Leaf
  schedule (Branch a la ra) = Branch <$> prepare (f a) <*> require la <*> require ra

  byLevel :: forall t. Traversable t => t (Tree a) -> f (t (Tree b))
  byLevel tta = if null tta -- need to check to prevent infinite recursion
    then -- tta is empty, so all this traversal does is alter the type and wrap it in f
         -- `pure (undefined <$> tta) works just as well
         fmap (fmap getLevelOrder . getCompose) . traverse f . Compose $ fmap LevelOrder tta 
    else traverse schedule tta `evalPlan` byLevel
      -}


-- | 'Tree' wrapper to use 'inorder' traversal
--
-- >>> mapM_ print . InOrder $ toDepth 3
-- [False,False]
-- [False]
-- [False,True]
-- []
-- [True,False]
-- [True]
-- [True,True]
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
-- [False]
-- [False,False]
-- [False,True]
-- [True]
-- [True,False]
-- [True,True]
--
-- >>> import Data.Foldable (toList)
-- >>> take 4 . toList $ PreOrder allDepths
-- [ [] , [ False ] , [ False , False ] , [ False , False , False ] ]
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
-- [True]
-- [True,True]
-- [True,False]
-- [False]
-- [False,True]
-- [False,False]
--
-- >>> import Data.Foldable (toList)
-- >>> take 4 . toList $ PostOrder allDepths
-- [ [] , [ True ] , [ True , True ] , [ True , True , True ] ]
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
-- [False]
-- [True]
-- [False,False]
-- [False,True]
-- [True,False]
-- [True,True]
--
-- >>> import Data.Foldable (toList)
-- >>> take 4 . toList $ LevelOrder allDepths
-- [ [] , [ False ] , [ True ] , [ False , False ] ]
newtype LevelOrder a = LevelOrder { getLevelOrder :: Tree a }
  deriving Functor
instance Foldable LevelOrder where
  foldMap = foldMapDefault
instance Traversable LevelOrder where
  traverse f = fmap LevelOrder . levelorder f . getLevelOrder
