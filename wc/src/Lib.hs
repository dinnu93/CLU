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
  let filesResultList = getFilesResultList actionList
  putStrLn $ displayFilesResultList filesResultList

-- All the Data Definitions

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

data FileResult = FileResult {fileResultName :: FileName,
                              fileResultList :: [Int]} deriving (Show)

data TotalResult = TotalResult [Int] deriving (Show)
data FilesResultList = FilesResultList [FileResult] TotalResult deriving (Show)

-- End of Data Definitons

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
          
optionDispatch :: File -> Option -> Int
optionDispatch (File _ fString fByteString) o
  | o == "-c" || o == "--bytes" = byteCount fByteString
  | o == "-m" || o == "--chars" = charCount fString
  | o == "-l" || o == "--lines" = lineCount fString
  | o == "-L" || o == "--max-line-length" = maxLineLength fString 
  | o == "-w" || o == "--words" = wordCount fString
  | otherwise = error "Not a valid option"


--Takes a File and an OptionList and gives the FileResult
getFileResult :: OptionList -> File -> FileResult
getFileResult opList f
  | opCount == 0 = getFileResult defaultOpList f
  | otherwise = FileResult (fileName f) $ map (optionDispatch f) (optionList opList)
  where opCount = optionCount opList
        defaultOpList = OptionList ["-l","-w","-c"] 3

-- Takes the ActionList and gives the result of all options on each file and gives a FileResultList 
getFilesResultList :: ActionList -> FilesResultList
getFilesResultList (ActionList opList fl) = FilesResultList fResultList (TotalResult tResultList)
  where fList = fileList fl
        opCount =  (optionCount opList)
        updatedOpCount = if opCount == 0 then 3 else opCount
        fResultList = map (getFileResult opList) fList
        tResultList = foldl (zipWith (+)) (replicate updatedOpCount 0) . map fileResultList $ fResultList 

-- Displaying the resultant file Result Lists and total Result List into string formats

-- Display one File Result 
displayFileResultList :: FileResult -> String
displayFileResultList fResult = L.intercalate " " displayList
  where displayList = map show (fileResultList fResult) ++ [fileResultName fResult]

-- Display Total Result
displayTotalResult :: TotalResult -> String
displayTotalResult (TotalResult tResultList) = L.intercalate " " displayList 
  where displayList = map show tResultList ++ ["total"]
          
-- Display the Total Files Result List
displayFilesResultList :: FilesResultList -> String
displayFilesResultList (FilesResultList fResultList totalResult)
  | fCount == 1 = fileResultDisplay
  | otherwise = fileResultDisplay ++ "\n" ++ totalResultDisplay
  where fCount = length fResultList
        fileResultDisplayList = map displayFileResultList fResultList
        fileResultDisplay = L.intercalate "\n" fileResultDisplayList
        totalResultDisplay = displayTotalResult totalResult
        
-- Individual functional units for each option

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

