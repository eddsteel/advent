{-# LANGUAGE OverloadedStrings #-}
module DayThree where

import Data.Text(Text)
import qualified Data.Text as T
import Data.Tuple.Curry(uncurryN)
import Data.List.Split
import Data.List(transpose)


-- | $setup
-- >>> :set -XOverloadedStrings

solve :: IO Text
solve = do
  t <- T.lines <$> T.pack <$> readFile "inputs/three.input"
  let process = T.pack . show . length . filter (uncurryN triangle)
  let answer1 = process $ readInputs t
  let answer2 = process $ readVertically t
  return $ T.concat [answer1, "\n", answer2]

listToTriple :: Read a => [String] -> (a, a, a)
listToTriple (a:b:c:[]) = (read a, read b, read c)
listToTriple _ = error "yarrr"

-- |
-- >>> readInputs ["   50   18  156"]
-- [(50,18,156)]
--
readInputs :: [Text] -> [(Int, Int, Int)]
readInputs = fmap $ listToTriple . (fmap T.unpack) . take 3 . T.words

-- |
-- >>> :{
-- take 1 $ readVertically [ "101 301 501\n"
--                         , "102 302 502\n"
--                         , "103 303 503\n"
--                         , "201 401 601\n"
--                         , "202 402 602\n"
--                         , "203 403 603\n"]
-- :}
-- [(101,102,103)]
readVertically :: [Text] -> [(Int, Int, Int)]
readVertically  = fmap listToTriple . ffmap T.unpack . concat . fmap transpose . ffmap T.words . chunksOf 3
  where ffmap f = fmap (fmap f)

-- |
-- >>> triangle 5 10 25
-- False
-- >>> triangle 2 3 4
-- True
--
triangle :: Int -> Int -> Int -> Bool
triangle a b c = a + b > c && b + c > a && c + a > b
