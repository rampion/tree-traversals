{-# LANGUAGE GADTs #-}
-- | Defines 'Phases', an 'Applicative' transformer for scheduling
-- effects during different phases of execution.
module Control.Applicative.Phases 
  ( Phases(..)
  , runPhasesForwards, runPhasesBackwards
  , now, later, delay
  ) where

import Control.Applicative (liftA2, (<**>))

-- | An applicative transformer to organize effects into an arbitrary number of
-- phases of execution.
--
-- Use 'now' to schedule actions for the current phase of execution:
--
-- >>> say name = putStrLn name *> pure name
-- >>> runPhasesForwards $ (,,) <$> now (say "Huey") <*> now (say "Dewey") <*> now (say "Louie")
-- Huey
-- Dewey
-- Louie
-- ("Huey","Dewey","Louie")
--
-- Or 'later' to schedule it for the next phase of execution:
--
-- >>> runPhasesForwards $ (,,) <$> later (say "Huey") <*> now (say "Dewey") <*> now (say "Louie")
-- Dewey
-- Louie
-- Huey
-- ("Huey","Dewey","Louie")
--
-- And 'delay' to delay a set of phased actions by one phase:
-- 
-- >>> runPhasesForwards $ delay ((,,) <$> later (say "Huey") <*> now (say "Dewey")) <*> now (say "Louie")
-- Louie
-- Dewey
-- Huey
-- ("Huey","Dewey","Louie")
--
-- Phases can also be run in reverse, but all actions in the same phase still occur in the same order:
--
-- >>> runPhasesBackwards $ (,,) <$> later (say "Huey") <*> now (say "Dewey") <*> now (say "Louie")
-- Huey
-- Dewey
-- Louie
-- ("Huey","Dewey","Louie")
data Phases f a where
  Lift :: f a -> Phases f a
  (:<*>) :: f (a -> b) -> Phases f a -> Phases f b

-- | run the phased actions in forwards order
--
-- >>> runPhasesForwards $ now (putStrLn "hello") *> later (putStrLn "world")
-- hello
-- world
-- >>> runPhasesForwards $ later (putStrLn "hello") *> now (putStrLn "world")
-- world
-- hello
runPhasesForwards :: Applicative f => Phases f a -> f a
runPhasesForwards (Lift ma) = ma
runPhasesForwards (mg :<*> tx) = mg <*> runPhasesForwards tx

-- | run the phased actions in backwards order
--
-- >>> runPhasesBackwards $ now (putStrLn "hello") *> later (putStrLn "world")
-- world
-- hello
-- >>> runPhasesBackwards $ later (putStrLn "hello") *> now (putStrLn "world")
-- hello
-- world
runPhasesBackwards :: Applicative f => Phases f a -> f a
runPhasesBackwards (Lift ma) = ma
runPhasesBackwards (mg :<*> tx) = runPhasesBackwards tx <**> mg

-- | schedule an action to run in the current phase
now :: f a -> Phases f a
now = Lift

-- | schedule  an action to run in the next phase
later :: Applicative f => f a -> Phases f a
later = delay . now

-- | delay all actions by a phase
delay :: Applicative f => Phases f a -> Phases f a
delay ta = pure id :<*> ta

instance Functor f => Functor (Phases f) where
  fmap f (Lift ma) = Lift (fmap f ma)
  fmap f (mg :<*> tx) = fmap (f.) mg :<*> tx

instance Applicative f => Applicative (Phases f) where
  pure = now . pure
  Lift mf <*> Lift ma = Lift $ mf <*> ma 
  Lift mf <*> (mh :<*> ty) = liftA2 (.) mf mh :<*> ty
  (mg :<*> tx) <*> Lift ma = liftA2 flip mg ma :<*> tx
  (mg :<*> tx) <*> (mh :<*> ty) = liftA2 (\g h ~(x,y) -> g x (h y)) mg mh :<*> liftA2 (,) tx ty
