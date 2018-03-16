{-# LANGUAGE ViewPatterns #-}
-- | Pretty printer for 'Data.Tree.Tree' from "Data.Tree".
module Data.Tree.PrettyPrinter
  ( prettyPrint
  ) where

import Data.List (foldl')
import Data.Tree

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
prettyPrint :: Show a => Tree a -> IO ()
prettyPrint = \t -> let (_,_,xs) = draw t in mapM_ putStrLn xs where

  draw :: Show a => Tree a -> (Int, Int, [String])
  draw (Node a []) = (aw, alw, [as]) where
    as = show a
    aw = length as
    alw = (aw - 1) `div` 2
  draw (Node a [t]) = (w, c, top : next : rest) where
    as = show a
    aw = length as
    alw = (aw - 1) `div` 2
    (tw,tc,tb) = draw t
    c = max alw tc
    ai = c - alw
    ti = c - tc
    w = max (ai + aw) (ti + tw)
    top = replicate ai ' ' ++ as
    next = replicate c ' ' ++ "│"
    rest = (replicate ti ' ' ++) <$> tb
  draw (Node a ts) = (w, c, top : next : rest) where
    as = show a
    aw = length as
    alw = (aw - 1) `div` 2
    (tw, reverse -> tcs, tb) = foldl' merge (-1, [], []) $ fmap draw ts
    tc = (head tcs + last tcs) `div` 2
    c = max alw tc
    ti = c - tc
    ai = c - alw
    w = max (ai + aw) (ti + tw)
    top = replicate ai ' ' ++ as
    rest = (replicate ti ' ' ++) <$> tb

    cs = (ti +) <$> tcs
    (dh : (reverse -> dl : (reverse -> di))) = zipWith (-) cs (0:cs)

    
    (xs,y:zs) = splitAt c $ concat
      [ replicate dh ' '
      , "┌"
      , concat $ [ replicate (d - 1) '─' ++ "┬" | d <- di ]
      , replicate (dl - 1) '─'
      , "┐"
      ]

    y' = case y of
      '┌' -> '├'
      '┐' -> '┤'
      '┬' -> '┼'
      '─' -> '┴'
      _   -> '?'

    next = xs ++ y':zs


  merge (tw, cs, tb) (w,c,b) =
    ( tw + 1 + w
    , (tw + 1 + c) : cs
    , meld (tw + 1) tb b
    )

  meld w (x:xs) (y:ys) = (take w (x ++ repeat ' ') ++ y) : meld w xs ys
  meld w [] ys = (replicate w ' ' ++) <$> ys
  meld _ xs [] = xs

