{-# LANGUAGE RecordWildCards #-}
-- | Tools for working with the 'TreeDiagram' monoid to draw tree diagrams.
module Data.Monoid.TreeDiagram
  ( TreeDiagram
  , showTreeDiagram
  , printTreeDiagram
  , singleton
  , subtree
  , width
  , height
  ) where

import Data.List (intersperse)
import Data.Semigroup (Semigroup(..))

-- | combine difference-strings into one
concatShowS :: [ShowS] -> ShowS
concatShowS = foldr (.) id

-- | repeat a character a given number of times
replicateChar :: Int -> Char -> ShowS
replicateChar n = concatShowS . replicate n . showChar

-- | A monoid for generating tree diagrams
data TreeDiagram = Empty | NonEmpty
  { graph       :: GraphEnvironment -> ShowS -- ^ the top line of the diagram
  , graphWidth  :: Int -- ^ number of characters in the graph
  , graphIndent :: Int -- ^ left whitespace needed to align graph with rows underneath
  , graphDedent :: Int -- ^ right whitespace needed to pad graph to maximum row width
  , rows        :: [(Int,ShowS)] -- ^ width and definition of each row under the graph line
  , leftLimit   :: (Int,Int) -- ^ index of lines without any left whitespace
  , rightLimit  :: (Int,Int) -- ^ index of lines of maximum row width
  }

-- | TreeDiagram settings when rendering the graph line
data GraphEnvironment = GraphEnvironment
  { isLeftmost  :: !Bool -- ^ whether this part of the graph is the leftmost part of the graph
  , isRightmost :: !Bool -- ^ whether this part of the graph is the rightmost part of the graph
  , uptickIndex :: !Int -- ^ index  of the uptick, relative to this part of the graph
  }

-- | render a tree diagram as a function that prepends a multi-line string
showTreeDiagram :: TreeDiagram -> ShowS
showTreeDiagram Empty = id
showTreeDiagram NonEmpty{..} = 
  let graphLine =
        replicateChar graphIndent ' ' .
        graph GraphEnvironment
          { isLeftmost = True
          , isRightmost = True
          , uptickIndex = graphWidth -- don't show the uptick
          }
      rowLines = snd <$> rows
  in concatShowS . intersperse (showChar '\n') $ graphLine : rowLines

-- | print a tree diagram
printTreeDiagram :: TreeDiagram -> IO ()
printTreeDiagram = putStrLn . ($[]) . showTreeDiagram

-- | draw a value as a simple, single-line tree diagram
--
-- >>> printTreeDiagram $ singleton 'a'
-- 'a'
singleton :: Show a => a -> TreeDiagram
singleton a = NonEmpty
  { graph = const $ shows a
  , graphWidth = length $ show a
  , graphIndent = 0
  , graphDedent = 0
  , rows = []
  , leftLimit = (0,0)
  , rightLimit = (0,0)
  }

-- |
-- '<>' composes two tree diagrams horizontally, connecting them via horizontal line
--
-- >>> printTreeDiagram $ singleton 'a' <> singleton 'b'
-- 'a'─'b'
instance Semigroup TreeDiagram
-- |
-- 'mempty' is the empty tree diagram
--
-- >>> printTreeDiagram mempty
-- <BLANKLINE>
-- >>> printTreeDiagram $ mempty <> singleton 'a' <> mempty
-- 'a'
instance Monoid TreeDiagram where
  mempty = Empty
  Empty `mappend` d = d
  d `mappend` Empty = d
  a `mappend` b = NonEmpty
    { graph = \o -> 
        let uptickIndex' = uptickIndex o - graphWidth a
            midline = if 0 <= uptickIndex' && uptickIndex' < graphPadding
                        then replicateChar uptickIndex' '─' . 
                             showChar '┴' . 
                             replicateChar (graphPadding - 1 - uptickIndex') '─'
                        else replicateChar graphPadding '─'
        in
        graph a o{ isRightmost = False } . 
        midline .
        graph b o{ isLeftmost = False, uptickIndex = uptickIndex' - graphPadding }
    , graphWidth = graphWidth a + graphPadding + graphWidth b
    , graphIndent = graphIndent a
    , graphDedent = graphDedent b
    , rows = alongside (width a + padding) (rows a) (rows b)
    , leftLimit = leftLimit a
    , rightLimit = rightLimit b
    }
    where graphPadding = graphDedent a + padding + graphIndent b
          padding = fromEnum (blo <= ahi && alo <= bhi)
          (alo,ahi) = rightLimit a
          (blo,bhi) = leftLimit b


-- | Full width of a tree diagram
--
-- >>> let d = singleton 'a' <> subtree (subtree (singleton 'b') <> singleton 'c')
-- >>> printTreeDiagram d
-- 'a'───┐
--       │
--     ┌─'c'
--     │
--    'b'
-- >>> width d
-- 9
width :: TreeDiagram -> Int
width Empty = 0
width d = graphIndent d + graphWidth d + graphDedent d

-- | Full height of a tree diagram
--
-- >>> let d = singleton 'a' <> subtree (subtree (singleton 'b') <> singleton 'c')
-- >>> printTreeDiagram d
-- 'a'───┐
--       │
--     ┌─'c'
--     │
--    'b'
-- >>> height d
-- 5
height :: TreeDiagram -> Int
height Empty = 0
height d = 1 + length (rows d)

-- | open zip of two blocks of text, padding the left block to a set width
alongside :: Int -> [(Int,ShowS)] -> [(Int,ShowS)] -> [(Int,ShowS)]
alongside n ((mx,dx):xs) ((my,dy):ys) = (n + my, dx . replicateChar (n - mx) ' ' . dy) : alongside n xs ys
alongside _ xs [] = xs
alongside n [] ys = [(n + my, replicateChar n ' '. dy) | (my,dy) <- ys]

-- | Pick a downtick character, based on the current environment.
downtick :: GraphEnvironment -> ShowS
downtick GraphEnvironment{..} = case (isLeftmost, isRightmost, uptickIndex == 0) of
  (False, False, False) -> showChar '┬'
  (False, False, True)  -> showChar '┼'
  (False, True, False)  -> showChar '┐'
  (False, True, True)   -> showChar '┤'
  (True, False, False)  -> showChar '┌'
  (True, False, True)   -> showChar '├'
  (True, True, False)   -> showChar '╷'
  (True, True, True)    -> showChar '│'

-- | Move a tree diagram to the subtree level, dropping a line
-- down from the graph line to connect it to the new toplevel.
--
-- >>> printTreeDiagram $ subtree (singleton 'a') <> singleton 'b' <> subtree (singleton 'c')
--  ┌─'b'─┐
--  │     │
-- 'a'   'c'
-- >>> printTreeDiagram $ subtree mempty
-- ╷
-- │
-- ╵
subtree :: TreeDiagram -> TreeDiagram
subtree Empty = NonEmpty
  { graph = downtick
  , graphWidth = 1
  , graphIndent = 0
  , graphDedent = 0
  , rows = [(1, showChar '│'),(1, showChar '╵')]
  , leftLimit = (1,2)
  , rightLimit = (1,2)
  }
subtree NonEmpty{..} = NonEmpty
  { graph = downtick
  , graphWidth = 1
  , graphIndent = uptickIndent
  , graphDedent = graphIndent + graphWidth + graphDedent - 1 - uptickIndent
  , rows  = (uptickIndent + 1, replicateChar uptickIndent ' ' . showChar '│') 
          : (graphIndent + graphWidth, replicateChar graphIndent ' ' . graphLine)
          : rows
  , leftLimit = (if llo > 1 then llo + 2 else if graphWidth > 1 then 2 else 1, lhi + 2)
  , rightLimit = (if rlo > 1 then rlo + 2 else if graphWidth > 2 then 2 else 1, rhi + 2)
  }
  where uptickIndent = graphIndent + uptickIndex
        uptickIndex = graphWidth `div` 2
        (llo,lhi) = leftLimit
        (rlo,rhi) = rightLimit
        graphLine = graph GraphEnvironment
          { isLeftmost = True
          , isRightmost = True
          , uptickIndex = uptickIndex
          }
