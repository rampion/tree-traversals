Similarity to Haxl
------------------
Maybe the README should mention a comparison to Haxl, perhaps
with the compression example?

Ad hoc traversals
-----------------
Is there a way to specify the order of traversal during
the traversal? How could this be implemented?

Typeclass approach
------------------
Currently, the tree-traversals package takes a pedagogical approach to
providing custom traversals.  It provides traversals for Tree, Forest, and
BinaryTree, and encourages users to use Batch to create their own traversals
for their custom tree data types, using the provided examples as inspiration.

A friendlier, and less repetitive approach could be to take a typeclass approach,
where we define something as tree-like if it contains a bitraversable of
subtrees and values:

```haskell
import "bifunctors" Data.Bitraversable
-- ...

class TreeLike tree where
  node :: tree a -> NodeLike tree a

-- | an existential lens, letting us focus on the subtrees and values
data NodeLike tree a = forall t. Bitraversable t => NodeLike
  { content :: t (tree a) a
  , context :: t (tree b) b -> tree b
  }

traverseWith :: TreeLike tree => (tree a -> f (tree b)) -> (a -> f b) -> tree a -> f (tree b)
traverseWith f g (node -> NodeLike {..}) = context <$> bitraverse f g content
```

For any instance of this class we can define `preorder`, `levelorder`, etc. just
using `traverseWith`:

```haskell
-- ...
inorder :: TreeLike tree => (a -> f b) -> tree a -> f (tree b)
inorder f = traverseWith (inorder f) f
  
preorder :: Treelike tree => (a -> f b) -> tree a -> f (tree b)
preorder f tree = traverseWith batch (lift . f) tree `runBatchWith` \nb ta ->
  nb <*> traverse (preorder f) ta
  
postorder :: Treelike tree => (a -> f b) -> tree a -> f (tree b)
postorder f tree = traverseWith batch (lift . f) tree `runBatchWith` \nb ta ->
  traverse (postorder f) ta <**> nb
  
levelorder :: Treelike tree => (a -> f b) -> tree a -> f (tree b)
levelorder f = topDown $ traverseWith batch (lift . f)

rlevelorder :: Treelike tree => (a -> f b) -> tree a -> f (tree b)
rlevelorder f = bottomUp $ traverseWith batch (lift . f)
```

And now we can define the various `newtype` wrappers just once:

```haskell
newtype InOrder tree a = InOrder { getInOrder :: tree a }
  deriving Functor
instance TreeLike tree => Foldable (InOrder tree) where
  foldMap = foldMapDefault
instance TreeLike tree => Traversable (InOrder tree) where
  traverse = inorder

-- ....
```

Now to use the traversals, `Tree`, `BinaryTree` or any other tree-like type
only needs to define an instance of `TreeLike`:

```haskell
import "bifunctors" Data.Bifunctor (Clown(..), Joker(..), Product(Pair), Tagged(..))
-- ...

instance TreeLike Tree where
  node (Node a as) = NodeLike
    { content = Tagged a `Pair` Clown as
    , context = \(Tagged b `Pair` Clown bs) -> Node b bs
    }

instance TreeLike BinaryTree where
  node Leaf = NodeLike
    { content = Joker Proxy
    , context = \(Joker Proxy) -> Leaf
    }
  node (Branch a l r) = NodeLike
    { content = Const l `Pair` Tagged a `Pair` Const r
    , context = (Const l `Pair` Tagged a `Pair` Const r) -> Branch a l r
    }
```

So this seems to be a good approach, with less repetition and less work for users.

The one thing that gives me pause is the awkwardness of defining a `TreeLike` instance for
`Forest`, which makes me want to refactor into a more complex type-family approach:

```haskell
class TreeLike (Sub tree) => TreeLike tree where
  type Sub tree a :: *
  node :: tree a -> NodeLike tree a 

data NodeLike tree a = forall t. Bitraversable t => NodeLike
  { content :: t (Sub tree a) a
  , context :: t (Sub tree b) b -> tree b
  }

traverseWith :: TreeLike tree => (Sub tree a -> f (Sub tree b)) -> (a -> f b) -> tree a -> f (tree b)
traverseWith f g (node -> NodeLike {..}) = context <$> bitraverse f g content

instance TreeLike Tree where
  type Sub Tree a = Tree a
  node (Node a as) = -- as before...

instance TreeLike BinaryTree where
  type Sub BinaryTree a = BinaryTree a
  node Leaf = -- as before...
  node (Branch a l r) = -- as before...

newtype Forest f tree a where Forest { getForest :: f (tree a) }

instance (Traversable f, TreeLike tree) => TreeLike (Forest f tree) where
  type Sub (Forest f tree) a = tree a
  node (Forest trees) = Node
    { content = Clown trees
    , context = \(Clown trees) -> Forest trees
    }
```

Maybe it makes sense to drop the bifunctors dependency:

```haskell
class TreeLike (Sub tree) => TreeLike tree where
  type Sub tree a :: *
  traverseWith :: (Sub tree a -> f (Sub tree b)) -> (a -> f b) -> tree a -> f (tree b)

instance TreeLike Tree where
  type Sub Tree a = Tree a
  traverseWith f g (Node a as) = Node <$> g a <*> traverse f as

instance TreeLike BinaryTree where
  type Sub BinaryTree a = BinaryTree a
  traverseWith _ _ Leaf = pure Leaf
  traverseWith f g (Branch a l r) = flip Branch <$> f l <*> g a <*> f r

newtype Forest f tree a where Forest { getForest :: f (tree a) }

instance (Traversable f, TreeLike tree) => TreeLike (Forest f tree) where
  type Sub (Forest f tree) a = tree a
  traverseWith f _ (Forest trees) = Forest <$> traverse f trees
```

Although, at this point maybe `traverseWith` should be renamed `treeTraverse` or `withTree`?
