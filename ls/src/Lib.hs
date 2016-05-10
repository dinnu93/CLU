module Lib
    ( someFunc
    ) where

import System.Directory as D
import Data.List as L

someFunc :: IO ()
someFunc = do
  d <- D.listDirectory "."
  (putStrLn . L.intercalate " " . L.sort) d

