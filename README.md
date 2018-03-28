The tree-traversals package defines [in-order, pre-order, post-order, level-order, and reversed level-order traversals](https://en.wikipedia.org/wiki/Tree_traversal#Types) for tree-like types:

```haskell
inorder, preorder, postorder, levelorder, rlevelorder
  :: (TreeLike tree, Applicative f) => (a -> f b) -> tree a -> f (tree b) 
```

The package also provides newtype wrappers for the various traversals so they
may be used with `traverse`, i.e.

```haskell
traverse f (InOrder tree) = inorder f tree
traverse f (PreOrder tree) = preorder f tree
traverse f (PostOrder tree) = postorder f tree
traverse f (LevelOrder tree) = levelorder f tree
traverse f (RLevelOrder tree) = rlevelorder f tree
```

To implement the various orders, the tree-traversals package provides the `Phases` applicative transformer for organizing effects into distinct phases.

Instances of `TreeLike` are provided for rose trees (`Tree` from [`Data.Tree`](http://hackage.haskell.org/package/containers/docs/Data-Tree.html)), binary trees (`BinaryTree` from this package's `Data.BinaryTree`), forests (`Forest` from this package's `Data.Traversable.TreeLike`),
and algebraic combinations of trees (`Compose outerTree innerTree`, `Product fstTree sndTree`, `Sum leftTree rightTree`).
