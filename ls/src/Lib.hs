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
  dirs <- D.listDirectory "."
  putStrLn $ argsDispatch args dirs
  
type Args = [String]
type Option = String 
 
argsDispatch :: Args -> [String] -> String
argsDispatch args dirList
  | length args == 0 = showDirCloumns . dirSort . rmDotFiles $ dirList -- Simple ls Command 

dirSort :: [String] -> [String]
dirSort = L.sortBy (\x y -> (map C.toLower x) `compare` (map C.toLower y))

rmDotFiles :: [String] -> [String] 
rmDotFiles = filter (\x -> not (head x == '.'))

showDirCloumns :: [String] -> String
showDirCloumns = L.intercalate " " 
