{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | All about traversing 'Tree's
module Tree where
-- rename Plan/Task -> BiPhase?
--  - Ask reddit before package release / after github upload
-- similarity to Haxl - compression example?
-- should I use Data.Tree?
-- support postlevelorder?
-- are adhoc traversals possible? - maybe. see bfs/dfs
--  - is there a way to blend the two?
--
-- data Direction = LeftBranch | RightBranch
--  - has Monoid instance preferring last
--  - useful for BFS/DFS
--
-- package structure:
--  Data.Tree.Binary
--  Data.Traversa{ble,l}.Tree
--  Data.Traversa{ble,l}.Tree.Binary
--  Control.Applicative.Trans.PlanT

import Control.Applicative (liftA2)
import Control.Arrow (first)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Traversable (foldMapDefault)

-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -interactive-print pPrint

-- | A binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show, Functor, Eq)

-- |
-- an infinite tree of all paths from the root ('False' for left, 'True' for right)
allDepths :: Tree [Bool]
allDepths = Branch [] (fmap (False:) allDepths) (fmap (True:) allDepths)

-- |
-- generate a finite version of 'allDepths' up to a certain depth
--
-- >>> toDepth 3
-- Branch
--   []
--   (Branch
--      [ False ]
--      (Branch [ False , False ] Leaf Leaf)
--      (Branch [ False , True ] Leaf Leaf))
--   (Branch
--      [ True ]
--      (Branch [ True , False ] Leaf Leaf)
--      (Branch [ True , True ] Leaf Leaf))
toDepth :: Int -> Tree [Bool]
toDepth 0 = Leaf
toDepth n = Branch [] (fmap (False:) t) (fmap (True:) t)
  where t = toDepth (n - 1)

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

-- |
-- A 'Task' splits the computation of @f a@ in two, one that is 'prepared' to be computed 
-- in advance and another that is dependent upon the response to some requirements.
data Task s req res f a = forall t. Traversable t => Task 
  { prepared :: f (t res -> (a, s res)) 
  , required :: s req -> t req
  }

instance Functor f => Functor (Task s req res f) where
  fmap f (Task prepared required) = Task (fmap (first f .) prepared) required

-- |
-- An Applicative transformer, each 'Plan' determines a single 'Task' from
-- context, with the benefit that 'Plan's can be combined via their
-- 'Applicative' instance
newtype Plan req res f a = Plan 
  { getPlan :: forall s. Traversable s => Task s req res f a }

-- if we had Plan req res f a ~ 
--  forall s. Traverable s -> s req -> exists t. Traversable t => (f (t res -> (a, s res)), t req)
--
--  we could define
--
-- compress :: (forall Traversable s. s req -> (t req, t res -> s res)) -> Plan req res f ()
--
-- but it's probably sufficient to do such compression/expansion wholesale during evalPlan

instance Functor f => Functor (Plan req res f) where
  fmap f (Plan ta) = Plan $ fmap f ta

instance Applicative f => Applicative (Plan req res f) where
  pure a = Plan $ Task
    { prepared = pure $ \result -> (a, result) 
    , required = id
    }

  pf <*> pa = Plan $ case getPlan pa of 
    Task prea reqa -> case getPlan pf of
      Task pref reqf -> Task
        { prepared = liftA2 apply pref prea
        , required = reqf . reqa
        }

-- | <*> for a shape-polymorphic indexed state monad 
apply :: (tf res -> (a -> b, ta res)) -> (ta res -> (a, s res)) -> (tf res -> (b, s res))
apply hf ha (hf -> (f, ha -> (a, result))) = (f a, result)

-- |
-- Like 'evalPlan', but allows you to specify an extra set of
-- requirements 
--
-- >>> helper s = putStrLn s >> return (length s)
-- >>> runPlan (pure 0) (traverse helper) ("hi" `Cons` "there" `Cons` Const ())
-- hi
-- there
-- ( 0 , 2 `Cons` (5 `Cons` Const ()) )
runPlan :: (Applicative f, Traversable s)
        => Plan req res f a
        -> (forall t. Traversable t => t req -> f (t res))  -- ^ how to resolve the task requirements
        -> s req                                            -- ^ a set of requirements unrelated to the task
        -> f (a, s res)
runPlan (Plan (Task prepared required)) query (query . required -> fresult) = prepared <*> fresult

-- |
-- Schedule this portion of the computation for the
-- first part of the 'Task'
--
-- >>> prepare (putStrLn "hi") `evalPlan` \t -> putStrLn "***" >> mapM print t
-- hi
-- *** 
prepare :: Functor f => f a -> Plan req res f a
prepare fa = Plan $ Task
  { prepared = (,) <$> fa
  , required = id
  }

-- |
-- State a requirement that must be fulfilled during
-- the second part of the 'Task'
--
-- >>> require "hi" `evalPlan` \t -> putStrLn "***" >> mapM print t
-- *** 
-- "hi"
require :: Applicative f => req -> Plan req res f res
require req = Plan $ Task
  { prepared = pure $ \(Cons res result) -> (res, result)
  , required = Cons req
  }

-- | 
-- Container that prepends an element to a traversable
data Cons s a = !a `Cons` !(s a) deriving (Show, Eq, Functor, Foldable, Traversable)
infixr 4 `Cons`

-- |
-- Compute the desired value in two stages, 
-- 
-- (1) first performing any prepared prepared actions,
-- (2) using the given callback to generate all the values required to complete
--     the computation 
--
-- >>> import Data.Char (toUpper)
-- >>> :{
--     before a = prepare $ do
--           putStrLn $ "before: " ++ show a
--           return [a,a]
--     after a = do
--           putStrLn $ "after: " ++ show a
--           return $ toUpper a
--     :}
--
-- >>> evalPlan ((,,) <$> before 'a' <*> before 'b' <*> before 'c') (traverse after)
-- before: 'a'
-- before: 'b'
-- before: 'c'
-- ( "aa" , "bb" , "cc" )
-- >>> evalPlan ((,,) <$> before 'a' <*> require 'b' <*> before 'c') (traverse after)
-- before: 'a'
-- before: 'c'
-- after: 'b'
-- ( "aa" , 'B' , "cc" )
-- >>> evalPlan ((,,) <$> require 'a' <*> before 'b' <*> require 'c') (traverse after)
-- before: 'b'
-- after: 'a'
-- after: 'c'
-- ( 'A' , "bb" , 'C' )
evalPlan :: Applicative f
         => Plan req res f a
         -> (forall t. Traversable t => t req -> f (t res))
         -> f a
evalPlan task query = fst <$> runPlan task query (Const ())

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
