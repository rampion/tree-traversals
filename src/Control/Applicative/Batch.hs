{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-- | Defines an 'Applicative' transformer, 'Batch' that allows you to postpone
-- certain side-effects to be computed as a batch.
--
-- Loosely speaking, a value of type @'Batch' query response f a@ is a
-- computation of type @f a@ with a hole of type @t query -> f (t response)@ in
-- it, for some 'Traversable' type @t@.
--
-- More strictly, as witnessed by 'fromBatch' and 'toBatch':
--
-- @
-- 'Batch' query response f a ~ forall r. (forall t. 'Traversable' t => f (t response -> a) -> t query -> r) -> r
-- @
-- 
-- Basic properties of 'Batch':
--
-- > fmap f m = pure f <*> m
-- > pure a = lift (pure a)
-- > lift x <*> lift y = lift (x <*> y)
-- > f <$> batch x <*> lift y = flip f <$> lift x <*> batch y
--  
-- In general, these allow you to permute any general expression
-- of type @'Batch' query response f a@ made of nothing but '<$>', 'pure', '<*>',
-- 'lift' and 'batch' into an expression of the form:
--
-- > lift x <*> batch y1 <*> batch y2 <*> ... <*> batch yN
--
-- Then we have
-- 
-- > runBatchWith (lift x <*> batch y1 <*> batch y2 <*> ... <*> batch yN) handler
-- >   = handler (uncurryN <$> x) (y1 `Cons` y2 `Cons` ... `Cons` yN `Cons` Nil)
--
-- where 
--
-- > class UncurryN t where
-- >   type Fun t a b :: *
-- >   uncurryN :: Fun t a b -> t a -> b
-- > 
-- > instance UncurryN Nil where
-- >   type Fun Nil a b = b
-- >   uncurryN f Nil = f
-- >   
-- > instance UncurryN t => UncurryN (Cons t)  where
-- >   type Fun (Cons t) a b = a -> Fun t a b
-- >   uncurryN f (a `Cons` ta) = uncurryN (f a) ta
--
module Control.Applicative.Batch 
  ( Batch(..), SomeBatch(..)
  , queryMap, responseMap, lift, batch
  , fromBatch, toBatch
  , runBatchWith, topDown, bottomUp, dedupe
  ) where

-- local variable name conventions:
--
-- prefixes:
--
--    b{} :: Batch query response f {}
--    f{} :: f {}
--    m{} :: f (t response -> ({}, s response))
--    g{} :: t response -> ({}, s response)
--    n{} :: f (t response -> {})
--    d{} :: s {} -> t {}
--    t{} :: t {}
--    s{} :: s {}
--
-- singletons/suffixes:
--
--    r :: response
--    q :: query
--    f :: a -> b
--
--

import Data.Foldable (foldl')
import Data.Functor.Product (Product(Pair))
import qualified Data.Map.Strict as Map -- "containers"
import Control.Applicative (liftA2,(<**>))
import Control.Arrow (first)

-- | An Applicative transformer, 'Batch' splits the computation of @f a@ in two,
--
-- * 'lifted' computations from @f a@
-- * a 'batched' traversable of queries @t query@ specifying a hole in the computation
--   for a value of type @f (t response)@.
newtype Batch query response f a = Batch 
  { getBatch :: forall s. Traversable s => SomeBatch query response f a s }

-- We could encode Batch as
--
-- > newtype Batch query response f a = Batch 
-- >   (  forall r s. Traversable s
-- >   => (  forall t. Traversable t 
-- >      => f (t response -> (a, s response))
-- >      -> (s query -> t query)
-- >      -> r
-- >      )
-- >   -> r
-- >   )
--
-- but caching in the Functor instance gets difficutlt

-- | The type used under the hood for 'Batch', to encode the existence
-- of some type @t@ for each given @s@.
data SomeBatch query response f a s = forall t. Traversable t => SomeBatch 
  { lifted :: f (t response -> (a, s response)) -- ^ indexed state transformer, to specify the hole
  , batched :: s query -> t query -- ^ difference-list encoding, for easy appending
  }

instance Functor f => Functor (Batch query response f) where
  fmap f ba = Batch $ case getBatch ba of 
    SomeBatch ma dq -> SomeBatch
      { lifted = (first f .) <$> ma
      , batched = dq
      }

instance Applicative f => Applicative (Batch query response f) where
  pure = lift . pure

  bf <*> ba = Batch $ case getBatch ba of 
    SomeBatch ma dq -> case getBatch bf of
      SomeBatch mf dq' -> SomeBatch
        { lifted = liftA2 apply mf ma
        , batched = dq' . dq
        }
    where
      -- | '<*>' for a shape-changing indexed state monad 
      apply :: (tf response -> (a -> b, ta response)) -> (ta response -> (a, s response)) -> (tf response -> (b, s response))
      apply gf ga (gf -> (f, ga -> (a, sr))) = (f a, sr)

-- | 'Batch' is covariant in its query parameter.
--
-- >>> :t queryMap fromEnum $ (+) <$> batch 'a' <*> batch 'b'
-- queryMap fromEnum $ (+) <$> batch 'a' <*> batch 'b'
--   :: (Num a, Applicative f) => Batch Int a f a
queryMap :: Functor f => (x -> y) -> Batch x response f a -> Batch y response f a
queryMap f (fromBatch -> k) = k $ \ma tx -> toBatch $ \k' -> k' ma $ fmap f tx

-- | 'Batch' is contravariant in its response parameter.
--
-- >>> :t responseMap fromEnum $ (+) <$> batch 'a' <*> batch 'b'
-- responseMap fromEnum $ (+) <$> batch 'a' <*> batch 'b'
--   :: (Applicative f, Enum a) => Batch Char a f Int
responseMap :: Functor f => (x -> y) -> Batch query y f a -> Batch query x f a
responseMap f (fromBatch -> k) = k $ \ma tq -> toBatch $ \k' -> k' ((. fmap f) <$> ma) tq


-- |
-- Lift an arbitrary computation into the 'Batch' transformer.
--
-- >>> lift (putStrLn "hi") `runBatchWith` \ma tq -> ma <* putStrLn "***" <*> mapM print tq
-- hi
-- *** 
lift :: Functor f => f a -> Batch query response f a
lift fa = Batch $ SomeBatch
  { lifted = (,) <$> fa
  , batched = id
  }

-- |
-- Add an item to the batched query that must be satisfied to run the 'Batch'
-- transformer:
--
-- >>> batch "hi" `runBatchWith` \ma tq -> ma <* putStrLn "***" <*> mapM print tq
-- *** 
-- "hi"
batch :: Applicative f => query -> Batch query response f response
batch q = Batch $ SomeBatch
  { lifted = pure $ \(Cons r sr) -> (r, sr)
  , batched = Cons q
  }

-- |
-- Computes @f a@ from @Batch query response f a@ given, for any 'Traversable' @t@,
--
-- - a way to compute @f (t response)@ from @t query@ (e.g. @'traverse' f@ for some @f@),
--
-- - an ordering of effects between the 'lifted' computation and the 'batched' computation
--   (e.g. '<*>' or '<**>').
--   
-- >>> import Data.Foldable (toList)
-- >>> data XYZ = X | Y | Z deriving Show
-- >>> :{
--     alone a = lift $ do
--       putStrLn $ "alone: " ++ show a
--       return $ Left a
--     handler ma tq = ma <*> do
--       putStrLn $ "batched: " ++ show (toList tq)
--       return $ Right <$> tq
--     :}
--
-- >>> runBatchWith ((,,) <$> alone X <*> alone Y <*> alone Z) handler
-- alone: X
-- alone: Y
-- alone: Z
-- batched: []
-- (Left X,Left Y,Left Z)
-- >>> runBatchWith ((,,) <$> alone X <*> batch Y <*> alone Z) handler
-- alone: X
-- alone: Z
-- batched: [Y]
-- (Left X,Right Y,Left Z)
-- >>> runBatchWith ((,,) <$> batch X <*> alone Y <*> batch Z) handler
-- alone: Y
-- batched: [X,Z]
-- (Right X,Left Y,Right Z)
runBatchWith
  :: Functor f
  => Batch query response f a
  -> (forall t. Traversable t => f (t response -> a) -> t query -> f a)
  -> f a
runBatchWith = fromBatch

-- | convert a @'Batch' query response f a@ into a continuation.
fromBatch
  :: Functor f
  => Batch query response f a
  -> (forall r. (forall t. Traversable t => f (t response -> a) -> t query -> r) -> r)
fromBatch (Batch (SomeBatch lifted batched)) handler = 
  handler (fmap fst <$> lifted) (batched Nil)

-- | convert a continuation into a @'Batch' query response f a@.
toBatch
  :: Functor f 
  => (forall r. (forall t. Traversable t => f (t response -> a) -> t query -> r) -> r)
  -> Batch query response f a
toBatch k = k $ \ma tq -> Batch $ SomeBatch
  { lifted = fmap (\ga (Pair tr sr) -> (ga tr, sr)) ma
  , batched = Pair tq
  }
  
-- | Given a way of computing @f b@ from @a@ dependent on solving zero or more
-- smaller subproblems of the shape @a -> f b@, recursively solve the subproblems
-- in from shallowest to deepest.
--
-- For example, consider the problem of finding the maximum value of a function
-- over a range of inputs:
--
-- >>> :{
--     maxInRange :: Ord a => (Int -> a) -> (Int,Int) -> Batch (Int,Int) a IO a
--     maxInRange f r@(lo,hi) = lift (print r) *> if lo == hi
--         then pure (f mid)
--         else max <$> batch (lo,mid) <*> batch (mid+1,hi)
--       where mid = (lo + hi) `div` 2
--     :}
--
-- >>> topDown (maxInRange id) (0,7)
-- (0,7)
-- (0,3)
-- (4,7)
-- (0,1)
-- (2,3)
-- (4,5)
-- (6,7)
-- (0,0)
-- (1,1)
-- (2,2)
-- (3,3)
-- (4,4)
-- (5,5)
-- (6,6)
-- (7,7)
-- 7
topDown :: forall a b f. Applicative f => (a -> Batch a b f b) -> a -> f b
topDown f = \a -> f a `runBatchWith` handler where
  handler :: forall t c. Traversable t => f (t b -> c) -> t a -> f c
  handler na ta = 
    na <*> 
    case castEmpty ta of
      Just tb -> pure tb -- avoid infinite recursion
      Nothing -> traverse f ta `runBatchWith` handler

-- | Given a way of computing @f b@ from @a@ dependent on solving zero or more
-- smaller subproblems of the shape @a -> f b@, recursively solve the subproblems
-- in from deepest to shallowest.
--
-- For example, consider the problem of finding the maximum value of a function
-- over a range of inputs:
--
-- >>> :{
--     maxInRange :: Ord a => (Int -> a) -> (Int,Int) -> Batch (Int,Int) a IO a
--     maxInRange f r@(lo,hi) = 
--       lift (print r) *> 
--       if lo == hi
--         then pure (f mid)
--         else max <$> batch (lo,mid) <*> batch (mid+1,hi)
--       where mid = (lo + hi) `div` 2
--     :}
--
-- >>> bottomUp (maxInRange id) (0,7)
-- (0,0)
-- (1,1)
-- (2,2)
-- (3,3)
-- (4,4)
-- (5,5)
-- (6,6)
-- (7,7)
-- (0,1)
-- (2,3)
-- (4,5)
-- (6,7)
-- (0,3)
-- (4,7)
-- (0,7)
-- 7
bottomUp :: forall a b f. Applicative f => (a -> Batch a b f b) -> a -> f b
bottomUp f = \a -> f a `runBatchWith` handler where
  handler :: forall t c. Traversable t => f (t b -> c) -> t a -> f c
  handler na ta =
    case castEmpty ta of
      Just tb -> pure tb -- avoid infinite recursion
      Nothing -> traverse f ta `runBatchWith` handler
    <**> na

-- | Cast the contents of a Traversable if it is empty.
--
-- >>> castEmpty ([1] :: [Int]) :: Maybe [Double]
-- Nothing
-- >>> castEmpty ([] :: [Int]) :: Maybe [Double]
-- Just []
castEmpty :: Traversable t => t a -> Maybe (t b)
castEmpty ta = case traverse (\a -> (pure a, bottom)) ta of 
    ([], tb) -> Just tb -- ta is empty, so we didn't need bottom to construct tb
    _        -> Nothing -- ta is non-empty
  where bottom = error "bottom"

-- | dedupe a batch operation handler
--
-- >>> decode c = putStrLn ("decoding " ++ show c) >> return (fromEnum c)
-- >>> traverse decode "Hello world"
-- decoding 'H'
-- decoding 'e'
-- decoding 'l'
-- decoding 'l'
-- decoding 'o'
-- decoding ' '
-- decoding 'w'
-- decoding 'o'
-- decoding 'r'
-- decoding 'l'
-- decoding 'd'
-- [72,101,108,108,111,32,119,111,114,108,100]
-- >>> dedupe (traverse decode) "Hello world"
-- decoding ' '
-- decoding 'H'
-- decoding 'd'
-- decoding 'e'
-- decoding 'l'
-- decoding 'o'
-- decoding 'r'
-- decoding 'w'
-- [72,101,108,108,111,32,119,111,114,108,100]
dedupe :: (Functor f, Ord query) 
       => (forall t. Traversable t => t query -> f (t response))
       -> (forall t. Traversable t => t query -> f (t response))
dedupe handler tq
  = fmap (\pr -> (pr Map.!) <$> tq) -- expand into original traversable
  . handler
  . foldl' (\pq q -> Map.insert q q pq) Map.empty -- compress into Map
  $ tq

-- | 
-- Container that prepends an element to a traversable
data Cons s a = !a `Cons` !(s a) deriving (Show, Functor, Foldable, Traversable)
infixr 4 `Cons`

-- |
-- Empty traversable
data Nil a = Nil deriving (Show, Functor, Foldable, Traversable)
