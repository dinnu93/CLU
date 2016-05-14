module Lib
    ( wcMain
    ) where

-- wc command in linux

import System.Environment
import qualified Data.ByteString as B 
import qualified Data.List as L

wcMain = do
  args <- getArgs
  let argList = argsParser args 
  let fileNames = listFileNames argList
  let options = listOptions argList
  fileStrings <- sequence . map readFile $ fileNames
  fileByteStrings <- sequence . map B.readFile $ fileNames
  let fileList = zipWith3 File fileNames fileStrings fileByteStrings
  let actionList = ActionList options (FileList fileList (length fileList))
        
  putStr "Hello"
  -- file <- readFile $ last args
  -- byteFile <- B.readFile $ last args
  -- putStrLn $ argsDispatch args file byteFile


type Args = [String]
type Option = String 
type FileName = FilePath 

data OptionList = OptionList {optionList :: [Option], optionCount :: Int} deriving (Show)
data FileNameList = FileNameList {fileNameList :: [FileName], fileNameCount :: Int} deriving (Show)
  
data ArgList = ArgList OptionList FileNameList deriving (Show) 

type FileString = String
type FileByteString = B.ByteString

data File = File {fileName :: FileName,
                  fileString :: FileString,
                  fileByteString :: FileByteString} deriving (Show)

data FileList = FileList {fileList :: [File],
                          fileCount :: Int} deriving (Show)

data ActionList = ActionList OptionList FileList deriving (Show)

--gives the list of filenames from an ArgList
listFileNames :: ArgList -> [FileName]
listFileNames (ArgList _ fNameList) = fileNameList fNameList

--gives the list of options with the option count 
listOptions :: ArgList -> OptionList
listOptions (ArgList opList _) = opList

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
sortOptions = L.intersect ["-l","--lines","-w","--words","-c","--bytes","-m","--chars","-L","--max-line-length"] . L.nub

-- filter options from the list of arguments, remove duplicates and sort them
-- according to wc option hierarchy order
filterOptions :: Args -> [Option]
filterOptions = sortOptions . filter optionCheck

-- filter file names from the args list 
filterFileNames :: Args -> [FileName]
filterFileNames = filter (not . optionCheck)

-- argsDispatch :: Args -> String -> B.ByteString -> String
-- argsDispatch args file byteFile
--   | length args == 1 = L.intercalate " " [(show . lineCount) file, (show . wordCount) file, (show . byteCount) byteFile, fileName]
--   | option == "-c" || option == "--bytes" = L.intercalate " " [(show . byteCount) byteFile, fileName]
--   | length args == 2 = L.intercalate " " [(show . (optionDispatch option)) file, fileName]
--   | otherwise = error "More arguments than required!"
--   where fileName = last args
--         option = args !! 0
          
optionDispatch :: Option -> File -> Int
optionDispatch o (File _ fString fByteString)
  | o == "-c" || o == "--bytes" = byteCount fByteString
  | o == "-m" || o == "--chars" = charCount fString
  | o == "-l" || o == "--lines" = lineCount fString
  | o == "-L" || o == "--max-line-length" = maxLineLength fString 
  | o == "-w" || o == "--words" = wordCount fString
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

