module Main where
import Data.List
import qualified Data.Map.Strict as Map
import System.IO

inputFile = "alastalon_salissa.txt"
outputFile = "alastalon_salissa_lyhennetty.txt"
maxLineLength = 80

main :: IO ()
main = do
  inputHandle <- openFile inputFile ReadMode
  hSetEncoding inputHandle utf8
  alastalo <- hGetContents inputHandle

  let result = reorganize alastalo
  
  outputHandle <- openFile outputFile WriteMode
  hSetEncoding outputHandle utf8
  hPutStr outputHandle result
  hClose outputHandle


{-
Reorganizes the words in the given String to fit to lines of fixed
length as tightly as possible.
-}
reorganize :: String -> String
reorganize = buildLines . foldr toMap Map.empty . words
  where
    toMap :: String -> Map.Map Int [String] -> Map.Map Int [String]
    toMap word map = Map.alter insertWord (length word) map
      where
        insertWord :: Maybe [String] -> Maybe [String]
        insertWord Nothing = Just [word]
        insertWord (Just xs) = Just (word:xs)


{-
Builds the lines of fixed length from the words in the given Map. The
Map should have word length as a key and a list of words of the same
length as a value.
-}
buildLines :: Map.Map Int [String] -> String
buildLines map
  | map == Map.empty = ""
  | otherwise = buildLines' map (0, "")
  where
    buildLines' :: Map.Map Int [String] -> (Int, String) -> String
    buildLines' map (lineLen, line)
      | map == Map.empty = line
      | otherwise = let
          space = if lineLen == 0 then "" else " "
          spaceLen = length space
          maxWordLen = maxLineLength - (lineLen + spaceLen)
          in case Map.lookupLE maxWordLen map of
               Nothing -> line ++ ("\n" ++ buildLines' map (0, ""))
               Just (wordLen, (word:others)) -> buildLines' newMap (newLen, newLine)
                 where
                   newMap = if others == []
                            then Map.delete wordLen map
                            else Map.adjust tail wordLen map
                   newLen = wordLen + spaceLen + lineLen
                   newLine = word ++ (space ++ line)
