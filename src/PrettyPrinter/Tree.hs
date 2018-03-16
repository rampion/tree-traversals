{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
-- | Pretty printer for 'Data.Tree.Tree' from "Data.Tree".
module PrettyPrinter.Tree where

import Data.Tree hiding (drawTree, drawForest)

-- |
-- >>> prettyPrint $ Node 'a' []
-- 'a'
-- >>> prettyPrint $ Node 'a' [Node 'b' []]
-- 'a'
--  │
-- 'b'
-- >>> prettyPrint $ Node 'a' [Node 'b' [], Node 'c' []]
--   'a'
--  ┌─┴─┐
-- 'b' 'c'
-- >>> prettyPrint $ Node 'a' [Node 'b' [], Node 'c' [], Node 'd' []]
--     'a'
--  ┌───┼───┐
-- 'b' 'c' 'd'
-- >>> prettyPrint $ Node "a" [Node "bbb" [Node "ccccc" []]]
--   "a"
--    │
--  "bbb"
--    │
-- "ccccc"
-- >>> prettyPrint $ Node "a" [Node "b" [], Node "c" [Node "d" []]]
--   "a"
--  ┌─┴─┐
-- "b" "c"
--      │
--     "d"
-- >>> t :: Tree [Int] ; t = Node [2] [Node [2,0] [], Node [2,1] [Node [2,1,0] []]]
-- >>> prettyPrint t
--     [2]
--   ┌──┴───┐
-- [2,0]  [2,1]
--          │
--       [2,1,0]
prettyPrint :: Show a => Tree a -> IO ()
prettyPrint = mapM_ putStrLn . block . drawTree

-- | the different types of drawings
data DrawingKind = Tree | Forest

-- | A monoid for composing tree or forest drawings
--
-- invariants:
--
-- - @left + center + right = maximum (length <$> block)@
-- - @center = 1 + sum deltas + length deltas@ if @block@ is non-null
data Drawing (k :: DrawingKind) = Drawing
  { left    :: Int      -- ^ width before first tree root
  , center  :: Int      -- ^ width spanning first to last tree root
  , right   :: Int      -- ^ width after last tree root
  , deltas  :: [Int]    -- ^ separation between tree roots 
  , block   :: [String] -- ^ lines to print
  }

-- | the monoid under horizontal concatenation
instance Monoid (Drawing 'Tree) where
  mempty = Drawing 0 0 0 [] []
  x `mappend` y | null (block x) = y
                | null (block y) = x
                | otherwise = Drawing
    { left = left x
    , center = center x + right x + 1 + left y + center y
    , right = right y
    , deltas = deltas x ++ right x + 1 + left y : deltas y
    , block = beside (width x + 1) (block x) (block y)
    }

-- | width of a drawing
width :: Drawing k -> Int
width x = left x + center x + right x

-- | horizontally concatenate the two given blocks, using
-- the left's given width to pad if necessary
beside :: Int -> [String] -> [String] -> [String]
beside w (x:xs) (y:ys) = (take w (x ++ repeat ' ') ++ y) : beside w xs ys
beside w [] ys = (replicate w ' ' ++) <$> ys
beside _ xs [] = xs

-- | generate a drawing of a tree
drawTree :: Show a => Tree a -> Drawing 'Tree
drawTree (Node (show -> as) (drawForest -> d)) = 
  let aw = length as
      dw = width d
      ac = (aw - 1) `div` 2
      dc = left d + (center d - 1) `div` 2
      c = max ac dc
      ai = c - ac
      di = c - dc
      w = max (ai + aw) (di + dw)
  in Drawing
    { left = c
    , center = 1
    , right = w - c - 1
    , deltas = []
    , block 
        = (replicate ai ' ' ++ as) :
          adjust (adjust uptick c) 0 ((replicate di ' ' ++) <$> block d)
    }

-- | change a graph character to include an uptick
uptick :: Char -> Char
uptick '┌' = '├'
uptick '┐' = '┤'
uptick '┬' = '┼'
uptick '╷' = '│'
uptick '─' = '┴'
uptick c = c

-- | transform the value at the given index in a list
adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i as = case splitAt i as of
  (xs,y:zs) -> xs ++ f y : zs
  _         -> as

-- | generate a drawing of a forest
drawForest :: Show a => Forest a -> Drawing 'Forest
drawForest [] = Drawing 0 0 0 [] []
drawForest (foldMap drawTree -> d) = d { block = graph : block d }
  where graph = replicate (left d) ' ' ++
                case concat [ '┬' : replicate n '─' | n <- deltas d ] of
                  []   -> "╷"
                  _:xs -> "┌" ++ xs ++ "┐"
