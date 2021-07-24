{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | By providing a 'TreeLike' instance, a functor can be traversed in several
-- orders:
--
-- ['inorder' / 'InOrder']
--    Viewing a 'TreeLike' functor as a sequence of values and subtrees, an
--    /__inorder__/ traversal iterates through this sequence visiting values and
--    traversing subtrees in the order they are given.
--
--        >>> printTree (label inorder exampleBinaryTree)
--              ┌──────6───┐
--              │          │
--          ┌──2┴───┐   ┌7─┴──┐
--          │       │   │     │
--        ┌0┴┐    ┌─┴5┐ ╵  ┌─9┴─┐
--        │  │    │   │    │    │
--        ╵ ┌1┐ ┌3┴┐  ╵   ┌8┐ ┌10┐
--          │ │ │  │      │ │ │  │
--          ╵ ╵ ╵ ┌4┐     ╵ ╵ ╵  ╵
--                │ │
--                ╵ ╵
--
-- ['preorder' / 'PreOrder']
--  Viewing a 'TreeLike' functor as a sequence of values and subtrees, a
--  /__preorder__/ traversal visits all the values in the sequence before
--  traversing the subtrees.
--
--        >>> printTree (label preorder exampleBinaryTree)
--              ┌──────0───┐
--              │          │
--          ┌──1┴───┐   ┌7─┴──┐
--          │       │   │     │
--        ┌2┴┐    ┌─┴4┐ ╵  ┌─8┴─┐
--        │  │    │   │    │    │
--        ╵ ┌3┐ ┌5┴┐  ╵   ┌9┐ ┌10┐
--          │ │ │  │      │ │ │  │
--          ╵ ╵ ╵ ┌6┐     ╵ ╵ ╵  ╵
--                │ │
--                ╵ ╵
--
-- ['postorder' / 'PostOrder']
--  Viewing a 'TreeLike' functor as a sequence of values and subtrees, a
--  /__postorder__/ traversal traverses all the subtrees in the sequence
--  before visiting the values in the sequence before
--  traversing the subtrees.
--
--        >>> printTree (label postorder exampleBinaryTree)
--              ┌──────10───┐
--              │           │
--          ┌──5┴───┐    ┌9─┴─┐
--          │       │    │    │
--        ┌1┴┐    ┌─┴4┐  ╵  ┌─8─┐
--        │  │    │   │     │   │
--        ╵ ┌0┐ ┌3┴┐  ╵    ┌6┐ ┌7┐
--          │ │ │  │       │ │ │ │
--          ╵ ╵ ╵ ┌2┐      ╵ ╵ ╵ ╵
--                │ │
--                ╵ ╵
--
-- ['levelorder' / 'LevelOrder']
--  Similar to a preorder traversal, a /__levelorder__/ traversal first visits
--  all the values at the root level before traversing any of the subtrees.
--  Instead of traversing the subtrees one by one, though, a levelorder
--  traversal interweaves their traversals, next visiting all the values at the
--  root of each subtree, then visiting all the values at the roots of each
--  subtree's subtrees, and so on. This is also known as a breadth-first
--  traversal.
--
--        >>> printTree (label levelorder exampleBinaryTree)
--               ┌──────0───┐
--               │          │
--          ┌──1─┴───┐   ┌2─┴─┐
--          │        │   │    │
--        ┌3┴┐    ┌──┴4┐ ╵  ┌─5─┐
--        │  │    │    │    │   │
--        ╵ ┌6┐ ┌7┴─┐  ╵   ┌8┐ ┌9┐
--          │ │ │   │      │ │ │ │
--          ╵ ╵ ╵ ┌10┐     ╵ ╵ ╵ ╵
--                │  │
--                ╵  ╵
--
-- ['rlevelorder' / 'RLevelOrder']
--  Similar to a postlevel traversal, a /__reversed levelorder__/ traversal
--  only visits all the values at the root level after traversing all of the
--  subtrees.  Instead of traversing the subtrees one by one, though, a
--  reversed levelorder traversal interweaves their traversals, working
--  from the deepest level up, though still in left-to-right order.
--
--        >>> printTree (label rlevelorder exampleBinaryTree)
--              ┌──────10───┐
--              │           │
--          ┌──8┴───┐    ┌9─┴─┐
--          │       │    │    │
--        ┌5┴┐    ┌─┴6┐  ╵  ┌─7─┐
--        │  │    │   │     │   │
--        ╵ ┌1┐ ┌2┴┐  ╵    ┌3┐ ┌4┐
--          │ │ │  │       │ │ │ │
--          ╵ ╵ ╵ ┌0┐      ╵ ╵ ╵ ╵
--                │ │
--                ╵ ╵
--
module Data.Traversable.TreeLike 
  ( TreeLike(..), treeFoldMap
  -- | = TreeLike wrappers
  -- These @newtype@s define 'TreeLike' instances for 'Traversable' types.
  , Forest(..), Flat(..), List(..)
  -- | = Traversals
  -- Each 'TreeLike' type admits multiple traversal orders:
  --
  -- > inorder, preorder, postorder, levelorder, rlevelorder
  -- >   :: TreeLike tree => Traversal (tree a) (tree b) a b
  --
  -- Using the definition of 'Control.Lens.Traversal.Traversal' from
  -- "Control.Lens.Traversal":
  --
  -- > type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t 
  --
  , inorder, preorder, postorder, levelorder, rlevelorder
  -- | = Traversable wrappers
  -- These @newtype@s define 'Traversable' instances for 'TreeLike' types.
  , InOrder(..), PreOrder(..), PostOrder(..), LevelOrder(..), RLevelOrder(..)
  -- | = Convenience functions
  , showTree, printTree
  ) where

import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Traversable (foldMapDefault)
import Data.Tree hiding (Forest)

import Control.Applicative.Phases
import Data.BinaryTree
import Data.Monoid.TreeDiagram
import Control.Comonad.Cofree (Cofree(..))
import Control.Monad.Free (Free(..))

-- | Render the tree as a string, using the 'TreeDiagram' monoid.
showTree :: (TreeLike tree, Show a) => tree a -> ShowS
showTree = showTreeDiagram . treeFoldMap singleton subtree

-- | Print the tree, using the 'TreeDiagram' monoid.
printTree :: (TreeLike tree, Show a) => tree a -> IO ()
printTree = putStrLn . ($[]) . showTree

-- | Notionally, functors are 'TreeLike' if any values and 'TreeLike'
-- substructure they contain can be traversed distinctly.
--
-- For example, given the 'TreeDiagram' monoid, one can use 'treeTraverse' with
-- the  'Const' applicative to recursively create a drawing of any tree,
-- rendering values inline with 'singleton' and dropping a line to drawings of
-- subtrees with 'subtree':
--
-- >>> :{
-- printTree :: (Show a, TreeLike tree) => tree a -> IO ()
-- printTree = printTreeDiagram . drawTree where
--   drawTree :: (Show a, TreeLike tree) => tree a -> TreeDiagram
--   drawTree = getConst . treeTraverse (Const . singleton) (Const . subtree . drawTree)
-- :}
--
-- This common pattern of mapping each element to a monoid and then modifying
-- each monoidal value generated from a subtree is captured by 'treeFoldMap', which
-- gives a slightly less verbose implementation of @printTree@.
--
-- >>> printTree = printTreeDiagram . treeFoldMap singleton subtree
--
-- Instances of 'TreeLike' are encouraged to avoid recursively defining
-- 'treeTraverse' in terms of itself, and to instead traverse subtrees
-- using the provided argument.
--
-- For example, given this definition for balanced binary trees:
--
-- >>> :{
-- data BBT a = Nil | a `Cons` BBT (a,a)
--   deriving Functor
-- infixr 4 `Cons`
-- :}
--
-- Its 'TreeLike' instance can be defined as:
--
-- >>> :{
-- instance TreeLike BBT where
--   treeTraverse = \f g t -> case t of
--      Nil          -> pure Nil
--      a `Cons` at  -> branch <$> g (fst <$> at) <*> f a <*> g (snd <$> at)
--    where
--      branch :: BBT b -> b -> BBT b -> BBT b
--      branch Nil b ~Nil = b `Cons` Nil
--      branch (x `Cons` xt) b ~(y `Cons` yt) = b `Cons` branch xt (x,y) yt
-- :}
--
-- This definition exposes the substructure in a way that can be used
-- by functions implemented in terms of 'treeTraverse', such as @printTree@:
--
-- >>> printTree $ 1 `Cons` (2,3) `Cons` ((4,5),(6,7)) `Cons` Nil
--    ┌───1───┐
--    │       │
--  ┌─2─┐   ┌─3─┐
--  │   │   │   │
-- ┌4┐ ┌6┐ ┌5┐ ┌7┐
-- │ │ │ │ │ │ │ │
-- ╵ ╵ ╵ ╵ ╵ ╵ ╵ ╵
class Functor tree => TreeLike tree where
  treeTraverse :: Applicative f 
               => (a -> f b)
               -> (forall subtree. TreeLike subtree => subtree a -> f (subtree b))
               -> tree a -> f (tree b)

-- | Recursively fold a tree into a monoid, using the given functions to
-- transform values and folded subtrees.
--
-- For example, one can find the maximum depth of a tree:
--
-- >>> printTree exampleTree
-- []─┬─────┬───────────┬─────────────────────────────┐
--    │     │           │                             │
--   [0] [1]┴─┐  [2]──┬─┴─────┐       [3]──┬───────┬──┴──────────────┐
--            │       │       │            │       │                 │
--          [1,0]   [2,0] [2,1]───┐      [3,0] [3,1]───┐   [3,2]───┬─┴────────┐
--                                │                    │           │          │
--                             [2,1,0]              [3,1,0]     [3,2,0] [3,2,1]────┐
--                                                                                 │
--                                                                             [3,2,1,0]
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> import GHC.Natural
-- >>> import Data.Semigroup
-- >>> :{
-- newtype Max = Max { getMax :: Natural } deriving (Num, Enum)
-- instance Semigroup Max where
--   (<>) = mappend
-- instance Monoid Max where
--   mempty = Max 0
--   Max a `mappend` Max b = Max $ a `max` b
-- :}
--
-- >>> getMax $ treeFoldMap (const 0) succ exampleTree
-- 4
treeFoldMap :: (Monoid m, TreeLike tree) => (a -> m) -> (m -> m) -> tree a -> m
treeFoldMap f g = getConst . treeTraverse (Const . f) (Const . g . treeFoldMap f g)

instance TreeLike Tree where
  treeTraverse f g (Node a as) = Node <$> f a <*> traverse g as

instance TreeLike BinaryTree where
  treeTraverse _ _ Leaf = pure Leaf
  treeTraverse f g (Branch l a r) = Branch <$> g l <*> f a <*> g r

instance Traversable f => TreeLike (Cofree f) where
  treeTraverse f g (a :< fa) = (:<) <$> f a <*> traverse g fa

instance Traversable f => TreeLike (Free f) where
  treeTraverse f g (Pure a) = Pure <$> f a
  treeTraverse f g (Free fa) = Free <$> traverse g fa

-- |
-- Use 'Product' to combine a pair of 'TreeLike' values into a single tree.
--
-- >>> smallBinaryTree = Branch (Branch Leaf [0,1] Leaf) [0] (Branch Leaf [0,2] Leaf)
-- >>> smallRoseTree = Node [1] [Node [1,0] [], Node [1,1] [], Node [1,2] [], Node [1,3] []]
-- >>> printTree $ Pair smallBinaryTree smallRoseTree
--         ┌────────────────────┐
--         │                    │
--    ┌───[0]───┐   [1]──┬─────┬┴────┬─────┐
--    │         │        │     │     │     │
-- ┌[0,1]┐   ┌[0,2]┐   [1,0] [1,1] [1,2] [1,3]
-- │     │   │     │
-- ╵     ╵   ╵     ╵
-- >>> visit a = StateT $ \e -> print a >> return (e, succ e)
-- >>> traversed <- postorder visit (Pair smallBinaryTree smallRoseTree) `evalStateT` 0
-- [0,1]
-- [0,2]
-- [0]
-- [1,0]
-- [1,1]
-- [1,2]
-- [1,3]
-- [1]
-- >>> printTree traversed
--    ┌───────┐
--    │       │
--  ┌─2─┐ 7┬─┬┴┬─┐
--  │   │  │ │ │ │
-- ┌0┐ ┌1┐ 3 4 5 6
-- │ │ │ │
-- ╵ ╵ ╵ ╵
instance (TreeLike fst, TreeLike snd) => TreeLike (Product fst snd) where
  treeTraverse _ g (Pair x y) = Pair <$> g x <*> g y

-- | Use 'Sum' to unify two different types of trees into a single type.
--
-- >>> smallBinaryTree = Branch (Branch Leaf [0,1] Leaf) [0] (Branch Leaf [0,2] Leaf)
-- >>> smallRoseTree = Node [1] [Node [1,0] [], Node [1,1] [], Node [1,2] [], Node [1,3] []]
-- >>> someTree b = if not b then InL smallBinaryTree else InR smallRoseTree
-- >>> :t someTree
-- someTree :: Num a => Bool -> Sum BinaryTree Tree [a]
-- >>> printTree (someTree False)
--         ╷
--         │
--    ┌───[0]───┐
--    │         │
-- ┌[0,1]┐   ┌[0,2]┐
-- │     │   │     │
-- ╵     ╵   ╵     ╵
-- >>> printTree (someTree True)
--             ╷
--             │
-- [1]──┬─────┬┴────┬─────┐
--      │     │     │     │
--    [1,0] [1,1] [1,2] [1,3]
instance (TreeLike left, TreeLike right) => TreeLike (Sum left right) where
  treeTraverse _ g (InL x) = InL <$> g x
  treeTraverse _ g (InR y) = InR <$> g y

-- |
-- A newtype wrapper to allow traversing an entire traversable of trees
-- simultaneously.
--
-- >>> printTree $ Forest exampleTrees
--  ┌─────┬───────────┬─────────────────────────────┐
--  │     │           │                             │
-- [0] [1]┴─┐  [2]──┬─┴─────┐       [3]──┬───────┬──┴──────────────┐
--          │       │       │            │       │                 │
--        [1,0]   [2,0] [2,1]───┐      [3,0] [3,1]───┐   [3,2]───┬─┴────────┐
--                              │                    │           │          │
--                           [2,1,0]              [3,1,0]     [3,2,0] [3,2,1]────┐
--                                                                               │
--                                                                           [3,2,1,0]
-- >>> visit a = StateT $ \e -> print a >> return (e, succ e)
-- >>> traversed <- levelorder visit (Forest exampleTrees) `evalStateT` 0
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
-- >>> printTree traversed
-- ┌──┬───┬────────┐
-- │  │   │        │
-- 0 1┤ 2┬┴─┐ 3┬──┬┴────┐
--    │  │  │  │  │     │
--    4  5 6┴┐ 7 8┴┐ 9─┬┴──┐
--           │     │   │   │
--          10    11  12 13┴┐
--                          │
--                         14
--
-- This is more of a convenience than a necessity, as @'Forest' t tree ~
-- 'Compose' ('Flat' t) tree@
--
-- >>> printTree . Compose $ Flat exampleTrees
--  ┌─────┬───────────┬─────────────────────────────┐
--  │     │           │                             │
-- [0] [1]┴─┐  [2]──┬─┴─────┐       [3]──┬───────┬──┴──────────────┐
--          │       │       │            │       │                 │
--        [1,0]   [2,0] [2,1]───┐      [3,0] [3,1]───┐   [3,2]───┬─┴────────┐
--                              │                    │           │          │
--                           [2,1,0]              [3,1,0]     [3,2,0] [3,2,1]────┐
--                                                                               │
--                                                                           [3,2,1,0]
newtype Forest t tree a = Forest { getForest :: t (tree a) }
  deriving Functor

instance (Traversable t, TreeLike tree) => TreeLike (Forest t tree) where
  treeTraverse _ g = fmap Forest . traverse g . getForest

-- |
-- A newtype wrapper for @[a]@ whose `TreeLike` instance
-- treats each cons-cell as a tree containing one value and one subtree.
--
-- >>> printTree $ List [1..5]
-- 1─┐
--   │
--  2┴┐
--    │
--   3┴┐
--     │
--    4┴┐
--      │
--     5┤
--      │
--      ╵
-- >>> import Data.Foldable (toList)
-- >>> toList . PostOrder $ List [1..5]
-- [5,4,3,2,1]
-- 
-- Contrast with @'Flat' [] a@:
--
-- >>> printTree $ Flat [1..5]
-- 1─2─3─4─5
-- >>> toList . PostOrder $ Flat [1..5]
-- [1,2,3,4,5]
--
newtype List a = List { getList :: [a] }
  deriving Functor

instance TreeLike List where
  treeTraverse f g (List as) = List <$> case as of
    []    -> pure []
    a:as  -> (:) <$> f a <*> (fmap getList . g .List) as


-- |
-- A newtype wraper for @t a@ whose `TreeLike` instance treats
-- the @t a@ as a flat structure with no subtrees.
--
-- >>> printTree $ Flat [1..5]
-- 1─2─3─4─5
-- >>> import Data.Foldable (toList)
-- >>> toList . PostOrder $ Flat [1..5]
-- [1,2,3,4,5]
newtype Flat t a = Flat { getFlat :: t a }
  deriving Functor

instance Traversable t => TreeLike (Flat t) where
  treeTraverse f _ (Flat ta) = Flat <$> traverse f ta


-- |
-- Treat subtrees and values of @outer (inner a)@ as subtrees of
-- @'Compose' outer inner a@.
--
-- For example 
--
-- >>> :{
-- exampleCompose =  Compose $ 
--    Branch 
--      (Branch Leaf (Node 'a' [Node 'b' [], Node 'c' [], Node 'd' []]) Leaf)
--      (Node 'e' [Node 'f' [Node 'g' [], Node 'h' []]]) 
--      (Branch Leaf (Node 'i' [Node 'i' [Node 'j' [Node 'k' []]]]) Leaf)
-- :}
-- 
-- >>> printTree exampleCompose
--         ┌─────────────┬───────────────┐
--         │             │               │
-- ┌───────┼───────┐ 'e'─┴──┐     ┌────┬─┴──────┐
-- │       │       │        │     │    │        │
-- ╵ 'a'─┬─┴─┬───┐ ╵    'f'─┼───┐ ╵ 'i'┴──┐     ╵
--       │   │   │          │   │         │
--      'b' 'c' 'd'        'g' 'h'     'i'┴─┐
--                                          │
--                                        'j'─┐
--                                            │
--                                           'k'
-- >>> treeFoldMap (const ["value"]) (const ["subtree"]) exampleCompose
-- ["subtree","subtree","subtree"]
instance (TreeLike outer, TreeLike inner) => TreeLike (Compose outer inner) where
  treeTraverse _ g (Compose trees) = Compose <$> treeTraverse g (fmap getCompose . g . Compose) trees
  
-- | Traverse all the values in a tree in left-to-right order.
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> visit a = StateT $ \e -> print a >> return (e, succ e)
-- >>> traversed <- inorder visit exampleBinaryTree `evalStateT` 0
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
-- >>> printTree traversed
--       ┌──────6───┐
--       │          │
--   ┌──2┴───┐   ┌7─┴──┐
--   │       │   │     │
-- ┌0┴┐    ┌─┴5┐ ╵  ┌─9┴─┐
-- │  │    │   │    │    │
-- ╵ ┌1┐ ┌3┴┐  ╵   ┌8┐ ┌10┐
--   │ │ │  │      │ │ │  │
--   ╵ ╵ ╵ ┌4┐     ╵ ╵ ╵  ╵
--         │ │
--         ╵ ╵
-- >>> printTree exampleTree
-- []─┬─────┬───────────┬─────────────────────────────┐
--    │     │           │                             │
--   [0] [1]┴─┐  [2]──┬─┴─────┐       [3]──┬───────┬──┴──────────────┐
--            │       │       │            │       │                 │
--          [1,0]   [2,0] [2,1]───┐      [3,0] [3,1]───┐   [3,2]───┬─┴────────┐
--                                │                    │           │          │
--                             [2,1,0]              [3,1,0]     [3,2,0] [3,2,1]────┐
--                                                                                 │
--                                                                             [3,2,1,0]
-- >>> traversed <- inorder visit exampleTree `evalStateT` 0
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
-- >>> printTree traversed
-- 0┬──┬───┬─────────┐
--  │  │   │         │
--  1 2┤ 4┬┴─┐ 8┬───┬┴─────┐
--     │  │  │  │   │      │
--     3  5 6┤  9 10┴┐ 12─┬┴──┐
--           │       │    │   │
--           7      11   13 14┴┐
--                             │
--                            15
inorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
inorder f = treeTraverse f (inorder f)

-- | Traverse all the values of a node, then recurse into each of its subtrees
-- in left-to-right order.
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> visit a = StateT $ \e -> print a >> return (e, succ e)
-- >>> traversed <- preorder visit exampleBinaryTree `evalStateT` 0
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
-- >>> printTree traversed
--       ┌──────0───┐
--       │          │
--   ┌──1┴───┐   ┌7─┴──┐
--   │       │   │     │
-- ┌2┴┐    ┌─┴4┐ ╵  ┌─8┴─┐
-- │  │    │   │    │    │
-- ╵ ┌3┐ ┌5┴┐  ╵   ┌9┐ ┌10┐
--   │ │ │  │      │ │ │  │
--   ╵ ╵ ╵ ┌6┐     ╵ ╵ ╵  ╵
--         │ │
--         ╵ ╵
-- >>> printTree exampleTree
-- []─┬─────┬───────────┬─────────────────────────────┐
--    │     │           │                             │
--   [0] [1]┴─┐  [2]──┬─┴─────┐       [3]──┬───────┬──┴──────────────┐
--            │       │       │            │       │                 │
--          [1,0]   [2,0] [2,1]───┐      [3,0] [3,1]───┐   [3,2]───┬─┴────────┐
--                                │                    │           │          │
--                             [2,1,0]              [3,1,0]     [3,2,0] [3,2,1]────┐
--                                                                                 │
--                                                                             [3,2,1,0]
-- >>> traversed <- inorder visit exampleTree `evalStateT` 0
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
-- >>> printTree traversed
-- 0┬──┬───┬─────────┐
--  │  │   │         │
--  1 2┤ 4┬┴─┐ 8┬───┬┴─────┐
--     │  │  │  │   │      │
--     3  5 6┤  9 10┴┐ 12─┬┴──┐
--           │       │    │   │
--           7      11   13 14┴┐
--                             │
--                            15
preorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
preorder f = runPhasesForwards . treeTraverse (now . f) (later . preorder f)

-- | Traverse all the values of a node after recursing into each of its
-- subtrees in left-to-right order.
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> visit a = StateT $ \e -> print a >> return (e, succ e)
-- >>> traversed <- postorder visit exampleBinaryTree `evalStateT` 0
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
-- >>> printTree traversed
--       ┌──────10───┐
--       │           │
--   ┌──5┴───┐    ┌9─┴─┐
--   │       │    │    │
-- ┌1┴┐    ┌─┴4┐  ╵  ┌─8─┐
-- │  │    │   │     │   │
-- ╵ ┌0┐ ┌3┴┐  ╵    ┌6┐ ┌7┐
--   │ │ │  │       │ │ │ │
--   ╵ ╵ ╵ ┌2┐      ╵ ╵ ╵ ╵
--         │ │
--         ╵ ╵
-- >>> printTree exampleTree
-- []─┬─────┬───────────┬─────────────────────────────┐
--    │     │           │                             │
--   [0] [1]┴─┐  [2]──┬─┴─────┐       [3]──┬───────┬──┴──────────────┐
--            │       │       │            │       │                 │
--          [1,0]   [2,0] [2,1]───┐      [3,0] [3,1]───┐   [3,2]───┬─┴────────┐
--                                │                    │           │          │
--                             [2,1,0]              [3,1,0]     [3,2,0] [3,2,1]────┐
--                                                                                 │
--                                                                             [3,2,1,0]
-- >>> traversed <- postorder visit exampleTree `evalStateT` 0
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
-- >>> printTree traversed
-- 15┬──┬───┬─────────┐
--   │  │   │         │
--   0 2┤ 6┬┴─┐ 14┬──┬┴────┐
--      │  │  │   │  │     │
--      1  3 5┤   7 9┤ 13─┬┴──┐
--            │      │    │   │
--            4      8   10 12┴┐
--                             │
--                            11
postorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
postorder f = runPhasesBackwards . treeTraverse (now . f) (later . postorder f)

-- | Traverse all the values of a tree in left-to-right breadth-first order.
-- (i.e. all nodes of depth @0@, then all nodes of depth @1@, then all nodes of
-- depth @2@, etc.)
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> visit a = StateT $ \e -> print a >> return (e, succ e)
-- >>> traversed <- levelorder visit exampleBinaryTree `evalStateT` 0
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
-- >>> printTree traversed
--        ┌──────0───┐
--        │          │
--   ┌──1─┴───┐   ┌2─┴─┐
--   │        │   │    │
-- ┌3┴┐    ┌──┴4┐ ╵  ┌─5─┐
-- │  │    │    │    │   │
-- ╵ ┌6┐ ┌7┴─┐  ╵   ┌8┐ ┌9┐
--   │ │ │   │      │ │ │ │
--   ╵ ╵ ╵ ┌10┐     ╵ ╵ ╵ ╵
--         │  │
--         ╵  ╵
-- >>> printTree exampleTree
-- []─┬─────┬───────────┬─────────────────────────────┐
--    │     │           │                             │
--   [0] [1]┴─┐  [2]──┬─┴─────┐       [3]──┬───────┬──┴──────────────┐
--            │       │       │            │       │                 │
--          [1,0]   [2,0] [2,1]───┐      [3,0] [3,1]───┐   [3,2]───┬─┴────────┐
--                                │                    │           │          │
--                             [2,1,0]              [3,1,0]     [3,2,0] [3,2,1]────┐
--                                                                                 │
--                                                                             [3,2,1,0]
-- >>> traversed <- levelorder visit exampleTree `evalStateT` 0
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
-- >>> printTree traversed
-- 0┬──┬───┬─────────┐
--  │  │   │         │
--  1 2┤ 3┬┴─┐ 4┬──┬─┴────┐
--     │  │  │  │  │      │
--     5  6 7┴┐ 8 9┴┐ 10─┬┴──┐
--            │     │    │   │
--           11    12   13 14┴┐
--                            │
--                           15
levelorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
levelorder = \f -> runPhasesForwards . schedule f where
  schedule :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> Phases f (tree b)
  schedule f = treeTraverse (now . f) (delay . schedule f)

-- | Traverse all the values of a tree in left-to-right inverse breadth-first order.
-- (i.e. all nodes of @n@, then all nodes of depth @n-1@, then all nodes of
-- depth @n-2@, etc.)
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> visit a = StateT $ \e -> print a >> return (e, succ e)
-- >>> traversed <- rlevelorder visit exampleBinaryTree `evalStateT` 0
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
-- >>> printTree traversed
--       ┌──────10───┐
--       │           │
--   ┌──8┴───┐    ┌9─┴─┐
--   │       │    │    │
-- ┌5┴┐    ┌─┴6┐  ╵  ┌─7─┐
-- │  │    │   │     │   │
-- ╵ ┌1┐ ┌2┴┐  ╵    ┌3┐ ┌4┐
--   │ │ │  │       │ │ │ │
--   ╵ ╵ ╵ ┌0┐      ╵ ╵ ╵ ╵
--         │ │
--         ╵ ╵
-- >>> printTree exampleTree
-- []─┬─────┬───────────┬─────────────────────────────┐
--    │     │           │                             │
--   [0] [1]┴─┐  [2]──┬─┴─────┐       [3]──┬───────┬──┴──────────────┐
--            │       │       │            │       │                 │
--          [1,0]   [2,0] [2,1]───┐      [3,0] [3,1]───┐   [3,2]───┬─┴────────┐
--                                │                    │           │          │
--                             [2,1,0]              [3,1,0]     [3,2,0] [3,2,1]────┐
--                                                                                 │
--                                                                             [3,2,1,0]
-- >>> traversed <- rlevelorder visit exampleTree `evalStateT` 0
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
-- >>> printTree traversed
-- 15─┬──┬─────┬────────┐
--    │  │     │        │
--   11 12┐ 13┬┴─┐ 14┬──┼────┐
--        │   │  │   │  │    │
--        5   6 7┤   8 9┤ 10┬┴─┐
--               │      │   │  │
--               1      2   3 4┤
--                             │
--                             0
rlevelorder :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> f (tree b)
rlevelorder = \f -> runPhasesBackwards . schedule f where
  schedule :: (Applicative f, TreeLike tree) => (a -> f b) -> tree a -> Phases f (tree b)
  schedule f = treeTraverse (now . f) (delay . schedule f)

-- | 'Tree' wrapper to use 'inorder' traversal
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> _ <- traverse print $ InOrder exampleBinaryTree
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
newtype InOrder tree a = InOrder { getInOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (InOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (InOrder tree) where
  traverse f = fmap InOrder . inorder f . getInOrder

-- | 'Tree' wrapper to use 'preorder' traversal
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> _ <- traverse print $ PreOrder exampleBinaryTree
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
newtype PreOrder tree a = PreOrder { getPreOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (PreOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (PreOrder tree) where
  traverse f = fmap PreOrder . preorder f . getPreOrder

-- | 'Tree' wrapper to use 'postorder' traversal
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> _ <- traverse print $ PostOrder exampleBinaryTree
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
newtype PostOrder tree a = PostOrder { getPostOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (PostOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (PostOrder tree) where
  traverse f = fmap PostOrder . postorder f . getPostOrder

-- | 'Tree' wrapper to use 'levelorder' traversal
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> _ <- traverse print $ LevelOrder exampleBinaryTree
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
newtype LevelOrder tree a = LevelOrder { getLevelOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (LevelOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (LevelOrder tree) where
  traverse f = fmap LevelOrder . levelorder f . getLevelOrder

-- | 'Tree' wrapper to use 'rlevelorder' traversal
--
-- >>> printTree exampleBinaryTree
--                     ┌──────────────────────[]────────┐
--                     │                                │
--      ┌─────────[L]──┴─────────────┐          ┌[R]────┴──────┐
--      │                            │          │              │
-- ┌[L,L]────┐              ┌────────┴──[L,R]┐  ╵       ┌────[R,R]────┐
-- │         │              │                │          │             │
-- ╵     ┌[L,L,R]┐   ┌[L,R,L]─────┐          ╵      ┌[R,R,L]┐     ┌[R,R,R]┐
--       │       │   │            │                 │       │     │       │
--       ╵       ╵   ╵       ┌[L,R,L,R]┐            ╵       ╵     ╵       ╵
--                           │         │
--                           ╵         ╵
-- >>> _ <- traverse print $ RLevelOrder exampleBinaryTree
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
newtype RLevelOrder tree a = RLevelOrder { getRLevelOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (RLevelOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (RLevelOrder tree) where
  traverse f = fmap RLevelOrder . rlevelorder f . getRLevelOrder

-- $setup
-- >>> :set -XDeriveFunctor
-- >>> import Control.Monad.State
-- >>> data Direction = L | R deriving Show
-- >>> :{
-- next :: a -> State Int Int
-- next = const . state $ \n -> (n, n+1)
-- label :: ((a -> State Int Int) -> tree a -> State Int (tree Int)) -> tree a -> tree Int
-- label traversal tree = traversal next tree `evalState` (0 :: Int)
-- :}
--
-- >>> :{
-- exampleTrees :: [Tree [Int]]
-- exampleTrees =
--   [ Node [0] []
--   , Node [1] [Node [1,0] []]
--   , Node [2] [Node [2,0] [], Node [2,1] [Node [2,1,0] []]]
--   , Node [3]
--     [ Node [3,0] []
--     , Node [3,1] [Node [3,1,0] []]
--     , Node [3,2] [Node [3,2,0] [], Node [3,2,1] [Node [3,2,1,0] []]]
--     ]
--   ]
-- exampleTree :: Tree [Int]
-- exampleTree = Node [] exampleTrees
-- exampleBinaryTree :: BinaryTree [Direction]
-- exampleBinaryTree = 
--   Branch
--   ( Branch
--       ( Branch
--           Leaf
--           [L,L]
--           (Branch Leaf [L,L,R] Leaf)
--       )
--       [L]
--       ( Branch
--           ( Branch 
--               Leaf
--               [L,R,L]
--               (Branch Leaf [L,R,L,R] Leaf)
--           )
--           [L,R]
--           Leaf
--       )
--   )
--   []
--   ( Branch
--       Leaf
--       [R]
--       ( Branch
--           (Branch Leaf [R,R,L] Leaf)
--           [R,R]
--           (Branch Leaf [R,R,R] Leaf)
--       )
--   )
-- :}
