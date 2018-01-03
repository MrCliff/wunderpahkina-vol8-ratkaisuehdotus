module Main where
-- import Prelude hiding (readFile, writeFile, length)
-- import System.IO.Encoding
-- import Data.Encoding.UTF8
-- import Prelude hiding (foldr)
import Data.List
import qualified Data.Map.Strict as Map
import System.IO

import Data.Time.Clock


maxRowLength = 80
inputFile = "alastalon_salissa.txt"
outputFile = "alastalon_salissa_lyhennetty.txt"

main :: IO ()
main = do
  -- let ?enc = UTF8
  alastalo <- readFile inputFile

  let result = jarjestaUudelleen alastalo
  writeFile outputFile result
  -- putStrLn result


bench :: IO () -> IO ()
bench action = do
  start <- getCurrentTime
  action
  end <- getCurrentTime

  putStrLn ("Action took: " ++ show (diffUTCTime end start))


v1 :: IO ()
v1 = do
  inputHandle <- openFile inputFile ReadMode
  hSetEncoding inputHandle utf8
  alastalo <- hGetContents inputHandle

  let result = jarjestaUudelleen1 alastalo
  
  outputHandle <- openFile outputFile WriteMode
  hSetEncoding outputHandle utf8
  hPutStr outputHandle result
  hClose outputHandle

  -- putStrLn result


v2 :: IO ()
v2 = do
  inputHandle <- openFile inputFile ReadMode
  hSetEncoding inputHandle utf8
  alastalo <- hGetContents inputHandle

  let result = jarjestaUudelleen alastalo
  
  outputHandle <- openFile outputFile WriteMode
  hSetEncoding outputHandle utf8
  hPutStr outputHandle result
  hClose outputHandle

  -- putStrLn result


jarjestaUudelleen :: String -> String
jarjestaUudelleen = buildLines . foldr toMap Map.empty . words
  where
    toMap :: String -> Map.Map Int [String] -> Map.Map Int [String]
    toMap word map = Map.alter insertWord (length word) map
      where
        insertWord :: Maybe [String] -> Maybe [String]
        insertWord Nothing = Just [word]
        insertWord (Just xs) = Just (word:xs)


buildLines :: Map.Map Int [String] -> String
-- buildLines empty = ""
buildLines map
  | map == Map.empty = ""
  | otherwise = buildLines' map (0, "")
  where
    buildLines' :: Map.Map Int [String] -> (Int, String) -> String
    -- buildLines' empty (_, line) = line
    buildLines' map (lineLen, line)
      | map == Map.empty = line
      | otherwise = let
          space = if lineLen == 0 then "" else " "
          spaceLen = length space
          maxWordLen = maxRowLength - (lineLen + spaceLen)
          in case Map.lookupLE maxWordLen map of
          -- in case findNextWord maxWordLen map of
               Nothing -> line ++ ("\n" ++ buildLines' map (0, ""))
               Just (wordLen, (word:others)) -> buildLines' newMap (newLen, newLine)
                 where
                   newMap = if others == []
                            then Map.delete wordLen map
                            else Map.adjust tail wordLen map
                   newLen = wordLen + spaceLen + lineLen
                   newLine = word ++ (space ++ line)
      
findNextWord :: Int -> Map.Map Int [String] -> Maybe (Int, [String])
findNextWord maxWordLen map = case Map.lookupLE maxWordLen map of
                                Nothing -> Nothing
                                Just result@(wordLen, words) -> if maxWordLen - wordLen == shortestWordLen
                                                                then findNextWord (wordLen - shortestWordLen) map
                                                                else Just result
                                  where
                                    (shortestWordLen, _) = Map.findMin map


      -- | newLen < maxRowLength = buildLines' updatedMap (newLen, newLine)
      -- | newLen == maxRowLength = newLine ++ "\n" ++ buildLines' updatedMap (0, "")
      -- | otherwise = 
      --   where
      --     newLen = lineLen + longestLen + (if lineLen == 0 then 0 else 1)
      --     newLine = (longestWord ++ (' ' : line))
      --     -- (firstLongest:restLongest) = lookupMax map
      --     (longestLen, longestWord, updatedMap) = case maxViewWithKey map of
      --                                                Nothing -> (0, "", empty)
      --                                                Just ((longestKey, (longest:otherLongest)), uMap) ->
      --                                                  (longestKey, longest, if otherLongest == []
      --                                                                        then uMap
      --                                                                        else insert longestKey otherLongest uMap)


--------------------------------------------------------------------------------


jarjestaUudelleen1 :: String -> String
-- jarjestaUudelleen1 xs = m . sortBy ordering . map lengths . words $ xs
jarjestaUudelleen1 xs = muodostaRivit . sortBy ordering . map lengths . words $ xs
  where
    lengths :: String -> (Int, String)
    lengths str = (length str, str)

    ordering :: (Int, String) -> (Int, String) -> Ordering
    ordering a b
      | fst a > fst b = LT -- The longest first
      | fst a < fst b = GT
      | otherwise = EQ

    m :: [(Int, String)] -> String
    m = concat . map (\x -> snd x ++ " ")


muodostaRivit :: [(Int, String)] -> String
muodostaRivit [] = []
muodostaRivit xs = snd (muodostaRivi xs (0, []))
  where --                                                  Modified      Result
    muodostaRivi :: [(Int, String)] -> (Int, String) -> ([(Int, String)], String)
    muodostaRivi [] (_, row) = ([], row)
    muodostaRivi (x:xs) (len, row)
      | newLen < maxRowLength = muodostaRivi xs (newLen, (word ++ (' ' : row))) -- row voi olla tyhjä
      | newLen == maxRowLength = let
          (leftover, readyLines) = muodostaRivi xs (0, [])
          in (leftover, word ++ (' ' : (row ++ ("\n" ++ readyLines))))
      | otherwise = let 
          (leftover, line) = muodostaRivi2 xs (len, row)
          leftoverWords = x:leftover
          (l, lines) = muodostaRivi leftoverWords (0, [])
          in (l, line ++ ("\n" ++ lines))
      where
        word = snd x
        wordLen = fst x
        newLen = len + wordLen + (if len == 0 then 0 else 1)

    muodostaRivi2 :: [(Int, String)] -> (Int, String) -> ([(Int, String)], String)
    muodostaRivi2 [] (_, row) = ([], row)
    muodostaRivi2 (x:xs) (len, row)
      | newLen < maxRowLength = muodostaRivi2 xs (newLen, (word ++ (' ' : row)))
      | newLen == maxRowLength = (xs, word ++ (' ' : (row)))
      | otherwise = let 
          (leftover, line) = muodostaRivi2 xs (len, row)
          leftoverWords = x:leftover
          in (leftoverWords, line)
      where
        word = snd x
        wordLen = fst x
        newLen = len + wordLen + (if len == 0 then 0 else 1)
