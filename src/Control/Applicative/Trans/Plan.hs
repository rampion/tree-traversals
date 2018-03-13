{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
-- | Defines an 'Applicative' transformer, 'Plan' that allows you to postpone
-- certain side-effects to be computed as a batch.
--
-- Basic properties of 'Plan':
--
-- @
-- fmap f m = pure f '<*>' m
-- pure a = prepare (pure a)
-- prepare x '<*>' prepare y = prepare (x '<*>' y)
-- f '<$>' require x '<*>' prepare y = flip f '<$>' prepare x '<*>' require y
-- @
--  
-- In general, these allow you to permute any general expression
-- of type @Plan req res f a@ made of nothing but '<$>', 'pure', '<*>',
-- 'prepare' and 'require' into an expression of the form:
--
-- @
-- prepare x '<*>' require y1 '<*>' require y2 '<*>' ... '<*>' require yN
-- @
--
-- Then we have
-- 
-- @
-- evalPlan (prepare x '<*>' require y1 '<*>' require y2 '<*>' ... '<*>' require yN) g
--   = uncurryN '<$>' x '<*>' g (y1 ``Cons`` y2 ``Cons`` ... ``Cons`` yN ``Cons`` Nil)
-- @
--
-- where 
--
-- @
-- class UncurryN t where
--   type Fun t a b :: *
--   uncurryN :: Fun t a b -> t a -> b
-- 
-- instance UncurryN Nil where
--   type Fun Nil a b = b
--   uncurryN f Nil = f
--   
-- instance UncurryN t => UncurryN (Cons t)  where
--   type Fun (Cons t) a b = a -> Fun t a b
--   uncurryN f (a ``Cons`` ta) = uncurryN (f a) ta
-- @
--
module Control.Applicative.Trans.Plan 
  ( Plan(..), evalPlan, runPlan, prepare, require
  , runWithQueue, traverseWithQueue
  , Task(..)
  ) where

import Control.Applicative (liftA2)
import Control.Arrow (first)

-- |
-- A 'Task' splits the computation of @f a@ in two, one that is 'prepared' to be computed 
-- in advance and another that is dependent upon the response to some requirements.
--
-- @f (t res -> a)@ and @t req@, waiting on @t req -> f (t res)@
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

-- | '<*>' for a shape-changing indexed state monad 
apply :: (tf res -> (a -> b, ta res)) -> (ta res -> (a, s res)) -> (tf res -> (b, s res))
apply hf ha (hf -> (f, ha -> (a, result))) = (f a, result)

-- |
-- Like 'evalPlan', but allows you to specify an extra set of
-- requirements 
--
-- >>> helper s = putStrLn s >> return (length s)
-- >>> runPlan (pure 0) (traverse helper) ("hi" `Cons` "there" `Cons` Nil)
-- hi
-- there
-- ( 0 , 2 `Cons` (5 `Cons` Nil) )
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
data Cons s a = !a `Cons` !(s a) deriving (Show, Functor, Foldable, Traversable)
infixr 4 `Cons`

-- |
-- Empty traversable
data Nil a = Nil deriving (Show, Functor, Foldable, Traversable)

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
evalPlan task query = fst <$> runPlan task query Nil


-- | Processes subproblems in FIFO order
--
runWithQueue :: Applicative f => (a -> Plan a b f b) -> a -> f b
runWithQueue f a = f a `evalPlan` traverseWithQueue f

traverseWithQueue :: (Traversable t, Applicative f) => (a -> Plan a b f b) -> t a -> f (t b)
traverseWithQueue f ta = case castEmpty ta of
  Just tb -> pure tb -- avoid infinite recursion
  Nothing -> traverse f ta `evalPlan` traverseWithQueue f

castEmpty :: Traversable t => t a -> Maybe (t b)
castEmpty ta = case traverse (\a -> (pure a, bottom)) ta of 
    ([], tb) -> Just tb -- ta is empty, so we didn't need bottom to construct tb
    _        -> Nothing -- ta is non-empty
  where bottom = error "bottom"

