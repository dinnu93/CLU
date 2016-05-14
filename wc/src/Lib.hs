module Lib
    ( wcMain
    ) where

-- wc command in linux

import System.Environment
import qualified Data.ByteString as B 
import qualified Data.List as L

wcMain = do
  args <- getArgs
  file <- readFile $ last args
  byteFile <- B.readFile $ last args
  putStrLn $ argsDispatch args file byteFile


type Args = [String]
type Option = String 
type FileName = String 

data OptionList = OptionList {optionList :: [Option], optionCount :: Int} deriving (Show)
data FileNameList = FileNameList {fileNameList :: [FileName], fileCount :: Int} deriving (Show)
  
data ArgList = ArgList OptionList FileNameList deriving (Show) 

-- Parse the arguments and convert it into a well sorted out ArgList data type
argsParser :: Args -> ArgList
argsParser ls =  ArgList (OptionList optionList (length optionList)) (FileNameList fileNameList (length fileNameList))
  where optionList = filterOptions ls
        fileNameList = filterFileNames ls

--checks if the given string is an option or not
optionCheck :: String -> Bool
optionCheck = L.isPrefixOf "-"

--sort options according to the wc's order of option hierarchy and remove
--duplicates
sortOptions :: [Option] -> [Option]
sortOptions = L.intersect ["-l","-w","-c","-m","-L"] . L.nub

-- filter options from the list of arguments, remove duplicates and sort them
-- according to wc option hierarchy order
filterOptions :: Args -> [Option]
filterOptions = sortOptions . filter optionCheck

-- filter file names from the args list 
filterFileNames :: Args -> [FileName]
filterFileNames = filter (not . optionCheck)

argsDispatch :: Args -> String -> B.ByteString -> String
argsDispatch args file byteFile
  | length args == 1 = L.intercalate " " [(show . lineCount) file, (show . wordCount) file, (show . byteCount) byteFile, fileName]
  | option == "-c" || option == "--bytes" = L.intercalate " " [(show . byteCount) byteFile, fileName]
  | length args == 2 = L.intercalate " " [(show . (optionDispatch option)) file, fileName]
  | otherwise = error "More arguments than required!"
  where fileName = last args
        option = args !! 0
          
optionDispatch :: Option -> String -> Int
optionDispatch c 
  | c == "-m" || c == "--chars" = charCount 
  | c == "-l" || c == "--lines" = lineCount 
  | c == "-L" || c == "--max-line-length" = maxLineLength 
  | c == "-w" || c == "--words" = wordCount 
  | otherwise = error "Not a valid option"
    
lineCount :: String -> Int
lineCount = length . lines

wordCount :: String -> Int
wordCount = length . words

charCount :: String -> Int
charCount = length

byteCount :: B.ByteString -> Int
byteCount = B.length

maxLineLength :: String -> Int
maxLineLength = maximum . (map charCount) . lines

