Ad hoc traversals
-----------------
Is there a way to specify the order of traversal during
the traversal? How could this be implemented?

```haskell
search :: TreeLike tree 
       => (forall r subtree. TreeLike subtree
          => Phases f (subtree b -> r)
          -> subtree a
          -> Phases f r
          )
       -> tree a
       -> f (tree b)
search f = runPhasesForwards . f (pure id)
```
