module Lib
    ( lsMain
    ) where

import System.Environment
import System.Directory as D
import Data.List as L
import Data.Char as C

lsMain :: IO ()
lsMain = do
  args <- getArgs
  putStrLn . L.intercalate " "  $ args

dirSort :: [String] -> [String]
dirSort = L.sortBy (\x y -> (map C.toLower x) `compare` (map C.toLower y))

rmDotFiles :: [String] -> [String] 
rmDotFiles = filter (\x -> not (head x == '.'))

showDirCloumns :: [String] -> String
showDirCloumns = L.intercalate " " . dirSort . rmDotFiles 
