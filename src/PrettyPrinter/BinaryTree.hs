-- | Pretty printer for 'Data.BinaryTree.BinaryTree' from "Data.BinaryTree".
module PrettyPrinter.BinaryTree
  ( prettyPrint
  ) where

import Data.BinaryTree

-- |
-- >>> prettyPrint Leaf
-- ●
-- >>> data Direction = L | R deriving Show
-- >>> prettyPrint (Branch [] Leaf $ Branch [R] Leaf $ Branch [R,R] Leaf Leaf)
--     []
-- ┌───┴───┐
-- ●      [R]
--      ┌──┴──┐
--      ●   [R,R]
--           ┌┴┐
--           ● ●
prettyPrint :: Show a => BinaryTree a -> IO ()
prettyPrint = \t -> let (_,_,xs) = draw t in mapM_ putStrLn xs where
  draw :: Show a => BinaryTree a -> (Int, Int, [String])
  draw Leaf = (1, 0, ["●"])
  draw (Branch a l r) = (w, c, top : next : outerMeld lb rb)
    where (lw,lc,lb) = draw l
          (rw,rc,rb) = draw r
          -- make both branches equidistant from the center
          rc' = max rc (lw - 1 - lc)
          rw' = rw - rc + rc'
          lw' = lc + 1 + rc'

          as = show a
          aw = length as
          alw = (aw - 1) `div` 2
          arw = aw - 1 - alw
          c = max lw' alw
          w = c + 1 + max rw' arw

          top = replicate (c - alw) ' ' ++ as
          next = concat
            [ replicate (c - lw' + lc) ' '
            , "┌"
            , replicate (lw' - 1 - lc) '─'
            , "┴"
            , replicate rc' '─'
            , "┐"
            ]
          meld ls rs = concat
            [ replicate (c - lw') ' '
            , take (lw' + 1) $ ls ++ repeat ' '
            , replicate (rc' - rc) ' '
            , rs
            ]
          outerMeld (x:xs) (y:ys) = meld x y : outerMeld xs ys
          outerMeld (x:xs) [] = meld x "" : outerMeld xs []
          outerMeld [] (y:ys) = meld "" y : outerMeld [] ys
          outerMeld [] [] = []
