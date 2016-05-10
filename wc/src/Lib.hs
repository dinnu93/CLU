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

