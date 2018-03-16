-- | Pretty printer for 'Data.Tree.Forest' from "Data.Tree".
module PrettyPrinter.Forest where

import Data.Tree hiding (drawForest)
import PrettyPrinter.Tree (Drawing(..), drawForest)

-- | 
-- >>> prettyPrint [Node 'a' []]
--  ╷
-- 'a'
-- >>> prettyPrint [Node [2,0] [], Node [2,1] [Node [2,1,0] []]]
--   ┌──────┐
-- [2,0]  [2,1]
--          │
--       [2,1,0]
prettyPrint :: Show a => Forest a -> IO ()
prettyPrint = mapM_ putStrLn . block . drawForest
