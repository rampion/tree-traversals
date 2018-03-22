{-# LANGUAGE RecordWildCards #-}
-- | Tools for working with the 'Drawing' monoid to draw tree diagrams.
module Drawing 
  ( Drawing
  , drawShow
  , uptick
  , putDrawing
  ) where

-- | a difference list, useful for appending
type DList a = [a] -> [a]

-- | prepend an item to a Difference list
prepend :: a -> DList a
prepend = (:)

-- | a difference list of characters
type DString = DList Char

-- | make a difference string by repeating a character a given number of times
replicateD :: Int -> Char -> DString
replicateD n c = (replicate n c ++)

-- | A monoid for generating tree diagrams
data Drawing = Blank | Drawing
  { graph       :: GraphEnvironment -> DString -- ^ the top line of the diagram
  , graphWidth  :: Int -- ^ number of characters in the graph
  , graphIndent :: Int -- ^ left whitespace needed to align graph with rows underneath
  , graphDedent :: Int -- ^ right whitespace needed to pad graph to maximum row width
  , rows        :: [(Int,DString)] -- ^ width and definition of each row under the graph line
  , leftLimit   :: (Int,Int) -- ^ index of lines without any left whitespace
  , rightLimit  :: (Int,Int) -- ^ index of lines of maximum row width
  }

-- | Drawing settings when rendering the graph line
data GraphEnvironment = GraphEnvironment
  { isLeftmost  :: !Bool -- ^ whether this part of the graph is the leftmost part of the graph
  , isRightmost :: !Bool -- ^ whether this part of the graph is the rightmost part of the graph
  , uptickIndex :: !Int -- ^ index  of the uptick, relative to this part of the graph
  }

-- | print a drawing, with the 'graph' on the top line and the 'rows' underneath.
putDrawing :: Drawing -> IO ()
putDrawing Blank = return ()
putDrawing Drawing{..} = do
  putStr $ replicate graphIndent ' '
  putStrLn $ flip graph [] GraphEnvironment
    { isLeftmost = True
    , isRightmost = True
    , uptickIndex = graphWidth
    }
  mapM_ (putStrLn . ($"") . snd) rows

-- | draw a value as a simple, single-line, drawing
--
-- >>> putDrawing $ drawShow 'a'
-- 'a'
drawShow :: Show a => a -> Drawing
drawShow a = Drawing
  { graph = const $ shows a
  , graphWidth = length $ show a
  , graphIndent = 0
  , graphDedent = 0
  , rows = []
  , leftLimit = (0,0)
  , rightLimit = (0,0)
  }

-- |
-- 'mempty' is the empty drawing, 'mappend' composes two drawings horizontally, connecting
-- them vial horizontal line.
--
-- >>> mempty `mappend` drawShow 'a' `mappend` mempty
-- 'a'
instance Monoid Drawing where
  mempty = Blank
  Blank `mappend` d = d
  d `mappend` Blank = d
  a `mappend` b = Drawing
    { graph = \o -> 
        let uptickIndex' = uptickIndex o - graphWidth a
            midline = if 0 <= uptickIndex' && uptickIndex' < graphPadding
                        then replicateD uptickIndex' '─' . 
                             prepend '┴' . 
                             replicateD (graphPadding - 1 - uptickIndex') '─'
                        else replicateD graphPadding '─'
        in
        graph a o{ isRightmost = False } . 
        midline .
        graph b o{ isLeftmost = False, uptickIndex = uptickIndex' - graphPadding }
    , graphWidth = graphWidth a + graphPadding + graphWidth b
    , graphIndent = graphIndent a
    , graphDedent = graphDedent b
    , rows = alongside (drawingWidth a + padding) (rows a) (rows b)
    , leftLimit = leftLimit a
    , rightLimit = rightLimit b
    }
    where graphPadding = graphDedent a + padding + graphIndent b
          padding = fromEnum (blo <= ahi && alo <= bhi)
          (alo,ahi) = rightLimit a
          (blo,bhi) = leftLimit b


-- | Full width of a drawing
drawingWidth :: Drawing -> Int
drawingWidth Blank = 0
drawingWidth d = graphIndent d + graphWidth d + graphDedent d

-- | open zip of two blocks of text, padding the left block to a set width
alongside :: Int -> [(Int,DString)] -> [(Int,DString)] -> [(Int,DString)]
alongside n ((mx,dx):xs) ((my,dy):ys) = (n + my, dx . replicateD (n - mx) ' ' . dy) : alongside n xs ys
alongside _ xs [] = xs
alongside n [] ys = [(n + my, replicateD n ' '. dy) | (my,dy) <- ys]

-- | Pick a downtick character, based on the current environment.
downtick :: GraphEnvironment -> DString
downtick GraphEnvironment{..} = case (isLeftmost, isRightmost, uptickIndex == 0) of
  (False, False, False) -> prepend '┬'
  (False, False, True)  -> prepend '┼'
  (False, True, False)  -> prepend '┐'
  (False, True, True)   -> prepend '┤'
  (True, False, False)  -> prepend '┌'
  (True, False, True)   -> prepend '├'
  (True, True, False)   -> prepend '╷'
  (True, True, True)    -> prepend '│'

-- | Add a line going vertically from the graph to a drawing
--
-- >>> uptick (drawShow 'a')
--  ╷
--  │
-- 'a'
-- >>> uptick mempty
-- ╷
-- │
-- ╵
uptick :: Drawing -> Drawing
uptick Blank = Drawing
  { graph = downtick
  , graphWidth = 1
  , graphIndent = 0
  , graphDedent = 0
  , rows = [(1, prepend '│'),(1, prepend '╵')]
  , leftLimit = (1,2)
  , rightLimit = (1,2)
  }
uptick Drawing{..} = Drawing
  { graph = downtick
  , graphWidth = 1
  , graphIndent = uptickIndent
  , graphDedent = graphIndent + graphWidth + graphDedent - 1 - uptickIndent
  , rows  = (uptickIndent + 1, replicateD uptickIndent ' ' . prepend '│') 
          : (graphIndent + graphWidth, replicateD graphIndent ' ' . graphLine)
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
