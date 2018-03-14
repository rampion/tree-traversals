{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-- | Defines an 'Applicative' transformer, 'Batch' that allows you to postpone
-- certain side-effects to be computed as a batch.
--
-- Basic properties of 'Batch':
--
-- > fmap f m = pure f <*> m
-- > pure a = lift (pure a)
-- > lift x <*> lift y = lift (x <*> y)
-- > f <$> batch x <*> lift y = flip f <$> lift x <*> batch y
--  
-- In general, these allow you to permute any general expression
-- of type @Batch q r f a@ made of nothing but '<$>', 'pure', '<*>',
-- 'lift' and 'batch' into an expression of the form:
--
-- > lift x <*> batch y1 <*> batch y2 <*> ... <*> batch yN
--
-- Then we have
-- 
-- > evalBatch (lift x <*> batch y1 <*> batch y2 <*> ... <*> batch yN) g
-- >   = uncurryN <$> x <*> g (y1 `Cons` y2 `Cons` ... `Cons` yN `Cons` Nil)
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
  ( Batch(..), SomeBatch(..), evalBatch, runBatch, lift, batch
  , runWithQueue, traverseWithQueue, dedupe
  ) where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map -- "containers"
import Control.Applicative (liftA2)
import Control.Arrow (first)

-- | An Applicative transformer, 'Batch' splits the computation of @f a@ in two,
--
-- * 'lifted' computations from @f a@
-- * a 'batched' traversable of queries @t q@ specifying a hole in the computation
--   for a value of type @f (t r)@.
newtype Batch q r f a = Batch 
  { getBatch :: forall s. Traversable s => SomeBatch q r f a s }

-- | The type used under the hood for 'Batch', as Haskell requires nested types to implement
-- @forall s. Traversable s => exists t. Traversable t => (f (t r -> (a, s r)), s q -> t q)@
data SomeBatch q r f a s = forall t. Traversable t => SomeBatch 
  { lifted :: f (t r -> (a, s r)) -- ^ indexed state transformer, to specify the hole
  , batched :: s q -> t q         -- ^ difference-list encoding, for easy appending
  }

instance Functor f => Functor (Batch q r f) where
  fmap f pa = Batch $ case getBatch pa of 
    SomeBatch ha qa -> SomeBatch
      { lifted = (first f .) <$> ha
      , batched = qa
      }

instance Applicative f => Applicative (Batch q r f) where
  pure = lift . pure

  pf <*> pa = Batch $ case getBatch pa of 
    SomeBatch ha qa -> case getBatch pf of
      SomeBatch hf qf -> SomeBatch
        { lifted = liftA2 apply hf ha
        , batched = qf . qa
        }
    where
      -- | '<*>' for a shape-changing indexed state monad 
      apply :: (tf r -> (a -> b, ta r)) -> (ta r -> (a, s r)) -> (tf r -> (b, s r))
      apply hf ha (hf -> (f, ha -> (a, result))) = (f a, result)

-- |
-- Like 'evalBatch', but allows you to specify an extra set of
-- requirements 
--
-- >>> helper s = putStrLn s >> return (length s)
-- >>> runBatch (pure 0) (traverse helper) ("hi" `Cons` "there" `Cons` Nil)
-- hi
-- there
-- ( 0 , 2 `Cons` (5 `Cons` Nil) )
runBatch :: (Applicative f, Traversable s)
        => Batch q r f a
        -> (forall t. Traversable t => t q -> f (t r))  -- ^ how to resolve the task requirements
        -> s q                                          -- ^ a set of requirements unrelated to the task
        -> f (a, s r)
runBatch (Batch (SomeBatch lifted batched)) query (query . batched -> fresult) = lifted <*> fresult

-- |
-- Schedule this portion of the computation for the
-- first part of the 'SomeBatch'
--
-- >>> lift (putStrLn "hi") `evalBatch` \t -> putStrLn "***" >> mapM print t
-- hi
-- *** 
lift :: Functor f => f a -> Batch q r f a
lift fa = Batch $ SomeBatch
  { lifted = (,) <$> fa
  , batched = id
  }

-- |
-- State a requirement that must be fulfilled during
-- the second part of the 'SomeBatch'
--
-- >>> batch "hi" `evalBatch` \t -> putStrLn "***" >> mapM print t
-- *** 
-- "hi"
batch :: Applicative f => q -> Batch q r f r
batch q = Batch $ SomeBatch
  { lifted = pure $ \(Cons r result) -> (r, result)
  , batched = Cons q
  }

-- | 
-- Container that prepends an element to a traversable
data Cons s a = !a `Cons` !(s a) deriving (Show, Functor, Foldable, Traversable)
infixr 4 `Cons`

-- |
-- Empty traversable
data Nil a = Nil deriving (Show, Functor, Foldable, Traversable)

-- |
-- Compute the desired value in two stages, 
-- 
-- (1) first performing any lifted lifted actions,
-- (2) using the given callback to generate all the values batched to complete
--     the computation 
--
-- >>> import Data.Char (toUpper)
-- >>> :{
--     before a = lift $ do
--           putStrLn $ "before: " ++ show a
--           return [a,a]
--     after a = do
--           putStrLn $ "after: " ++ show a
--           return $ toUpper a
--     :}
--
-- >>> evalBatch ((,,) <$> before 'a' <*> before 'b' <*> before 'c') (traverse after)
-- before: 'a'
-- before: 'b'
-- before: 'c'
-- ( "aa" , "bb" , "cc" )
-- >>> evalBatch ((,,) <$> before 'a' <*> batch 'b' <*> before 'c') (traverse after)
-- before: 'a'
-- before: 'c'
-- after: 'b'
-- ( "aa" , 'B' , "cc" )
-- >>> evalBatch ((,,) <$> batch 'a' <*> before 'b' <*> batch 'c') (traverse after)
-- before: 'b'
-- after: 'a'
-- after: 'c'
-- ( 'A' , "bb" , 'C' )
evalBatch :: Applicative f
         => Batch q r f a
         -> (forall t. Traversable t => t q -> f (t r))
         -> f a
evalBatch task query = fst <$> runBatch task query Nil


-- | Processes subproblems in FIFO order
--
runWithQueue :: Applicative f => (a -> Batch a b f b) -> a -> f b
runWithQueue f a = f a `evalBatch` traverseWithQueue f

traverseWithQueue :: (Traversable t, Applicative f) => (a -> Batch a b f b) -> t a -> f (t b)
traverseWithQueue f ta = case castEmpty ta of
  Just tb -> pure tb -- avoid infinite recursion
  Nothing -> traverse f ta `evalBatch` traverseWithQueue f

castEmpty :: Traversable t => t a -> Maybe (t b)
castEmpty ta = case traverse (\a -> (pure a, bottom)) ta of 
    ([], tb) -> Just tb -- ta is empty, so we didn't need bottom to construct tb
    _        -> Nothing -- ta is non-empty
  where bottom = error "bottom"

-- | dedupe a batch handler
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
-- [ 72 , 101 , 108 , 108 , 111 , 32 , 119 , 111 , 114 , 108 , 100 ]
-- >>> dedupe (traverse decode) "Hello world"
-- decoding ' '
-- decoding 'H'
-- decoding 'd'
-- decoding 'e'
-- decoding 'l'
-- decoding 'o'
-- decoding 'r'
-- decoding 'w'
-- [ 72 , 101 , 108 , 108 , 111 , 32 , 119 , 111 , 114 , 108 , 100 ]
dedupe :: (Functor f, Ord q) 
       => (forall t. Traversable t => t q -> f (t r))
       -> (forall t. Traversable t => t q -> f (t r))
dedupe f tq = fmap (\mr -> (mr Map.!) <$> tq) -- expand into original traversable
            . f
            . foldl' (\mq q -> Map.insert q q mq) Map.empty -- compress into Map
            $ tq
