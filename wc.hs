import System.Environment
import qualified Data.ByteString as B 

main = do
  args <- getArgs
  file <- readFile $ args !! 0
  byteFile <- B.readFile $ args !! 0
  putStr $ " " ++ (show $ lineCount file) ++ " "
  putStr $ (show $ wordCount file) ++ " "
  putStr $ (show $ maxLineLength file) ++ " "
  putStr $ (show $ byteCount byteFile) ++ " "
  putStrLn $ args !! 0 

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
