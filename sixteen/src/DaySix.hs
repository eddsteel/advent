module DaySix(solve) where

import Data.Text(Text)
import qualified Data.Text as T
import Data.Map.Strict(Map, fromList, toList, insertWith)
import Data.List(transpose, sortOn)
import Control.Arrow((>>>), (<<<))

solve :: IO String
solve = do
  input <- readFile "inputs/six.input"
  return $ T.unpack . T.unlines $ [solveStarOne input, solveStarTwo input]

-- |
-- >>> solveStarOne $ unlines ["eedadn","drvtee","eandsr","raavrd","atevrs","tsrnev","sdttsa","rasrtv","nssdts","ntnada","svetve","tesnvt","vntsnd","vrdear","dvrsen","enarar"]
-- "easter"
solveStarOne :: String -> Text
solveStarOne = solveAStar (byCommon last)

-- |
-- >>> solveStarTwo $ unlines ["eedadn","drvtee","eandsr","raavrd","atevrs","tsrnev","sdttsa","rasrtv","nssdts","ntnada","svetve","tesnvt","vntsnd","vrdear","dvrsen","enarar"]
-- "advent"
solveStarTwo :: String -> Text
solveStarTwo = solveAStar (byCommon head)

solveAStar :: (String -> Char) -> String -> Text
solveAStar f = T.pack . fmap f . transpose . lines

byCommon :: ([(Char, Int)] -> (Char, Int)) -> String -> Char
byCommon f = fst . f . sortOn snd . toList . count

-- |
-- >>> count "yoson"
-- fromList [('n',1),('o',2),('s',1),('y',1)]
count :: String -> Map Char Int
count = foldr addOrInc (fromList [])

-- |
-- >>> addOrInc 'i' (fromList [])
-- fromList [('i',1)]
addOrInc :: Char -> Map Char Int -> Map Char Int
addOrInc c = insertWith (+) c 1
