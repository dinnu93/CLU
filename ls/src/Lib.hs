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
  dirList <- D.getDirectoryContents "."
  putStrLn $ argsDispatch args dirList
  
type Args = [String]
type Option = String 

-- argsDispatch :: dispatches relevant operations to arguments              
argsDispatch :: Args -> [String] -> String
argsDispatch args dirList
  | length args == 0 = showDirCloumns . dirSort . rmDotFiles $ dirList -- Simple ls Command without any options
  | length args == 1 && optionCheck o1 = optionsDispatch o1 $ dirList -- ls [option]
  where o1 = (args !! 0)
        o2 = (args !! 1)

-- optionsDispatch :: dispatches relevant functionality for options
optionsDispatch :: Option -> [String] -> String
optionsDispatch o
  | o == "-a" || o == "--all" = showDirCloumns . dirSort  
  | o == "-A" || o == "--almost-all" = showDirCloumns . drop 2 . dirSort
  
-- dirSort :: sorts the files and directories in alphabetical order
dirSort :: [String] -> [String]
dirSort = L.sortBy (\x y -> (map C.toLower (rmDot x)) `compare` (map C.toLower (rmDot y)))

--removes dot infront of a string if it has one
rmDot :: String -> String
rmDot s
  | head s == '.' = tail s
  | otherwise = s
    
-- rmDotFiles :: removes the hidden and ignored files and directories which start
-- with a dot
rmDotFiles :: [String] -> [String] 
rmDotFiles = filter (\x -> not (head x == '.'))

-- showDirCloumns :: displays all the files and directories in columns  
showDirCloumns :: [String] -> String
showDirCloumns = L.intercalate " " 

-- optionCheck :: checks if the given argument is an option or not
optionCheck :: String -> Bool
optionCheck o = (head o) == '-'
