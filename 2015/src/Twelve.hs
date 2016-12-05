{-# LANGUAGE OverloadedStrings #-}
module Twelve where

import Data.Char(isNumber)
import Data.Maybe(maybeToList)
import Data.Text(Text)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Aeson(Value(..),Value,decode)
import Data.Scientific(Scientific, toBoundedInteger)
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

getNumbers :: Value -> [Int]
getNumbers (Number i) = (maybeToList . toBoundedInteger) i -- we're ok with our inputs
getNumbers (Array a) = concat $ fmap getNumbers a
getNumbers (Object h) = concat $ fmap getNumbers (M.elems h)
getNumbers _ = []

getNumbersWR :: Value -> [Int]
getNumbersWR (Number i) = (maybeToList . toBoundedInteger) i -- we're ok with our inputs
getNumbersWR (Array a) = concat $ fmap getNumbersWR a
getNumbersWR (Object h)
  | "red" `elem` M.elems h = []
  | otherwise = concat $ fmap getNumbersWR (M.elems h)
getNumbersWR _ = []


-- | sum the numbers in a string (that contains a bunch of other characters)
--
--
-- >>> nums "[1,2,3]"
-- [1,2,3]
--
-- >>> nums "[[[3]]]"
-- [3]
--
-- >>> nums "{\"a\":{\"b\":4},\"c\":-1}"
-- [4,-1]
--
-- {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
-- [] and {} both have a sum of 0.
-- You will not encounter any strings containing numbers.
--
nums :: String -> [Int]
nums s = (concat . maybeToList) (fmap getNumbers v)
  where v = decode (BL.pack s) :: Maybe Value


-- | Nums without red
--
-- >>> numswr "{\"a\":{\"ba\":[1,2,3],\"bb\":\"red\"},\"c\":-1}"
-- [-1]
--
-- >>> numswr "{\"e\":\"violet\",\"c\":123,\"a\":101,\"b\":87,\"d\":\"red\",\"f\":88}"
-- []
--
numswr :: String -> [Int]
numswr s = (concat . maybeToList) (fmap getNumbersWR v)
  where v = decode (BL.pack s) :: Maybe Value

starOne :: String-> String
starOne s = (show . sum . concat) $ fmap nums (lines s)

starTwo :: String -> String
starTwo s = (show . sum . concat) $ fmap numswr (lines s)


------------------------------- Naive string filtering way that worked for star one

-- | Extract contiguous sections of a list filtered by the supplied predicates.
--
-- >>> extract isNumber isNumber "[1,2,3]"
-- ["1","2","3"]
--
-- >>> extract (\x -> isNumber x || (x =='-')) isNumber "{\"a\": [-1,240]}"
-- ["-1","240"]
--
extract :: Eq a => (a -> Bool) -> (a -> Bool) -> [a] -> [[a]]
extract _ _ [] = []
extract f g s = pfx f g s : (filter (/= []) (extract f g (sfx f g s)))
                             -- TODO ^ hack ^


pfx f g s = ppfx s ++ psfx s
  where
    ppfx = take 1 . dropWhile (not . f)
    psfx = takeWhile g . drop 1 . dropWhile (not . f)

sfx f g = (dropWhile g . drop 1 . dropWhile (not . f))


-- | sum the numbers in a string (that contains a bunch of other characters)
--
--
-- >>> sumNums' "[1,2,3]"
-- 6
--
-- >>> sumNums' "[[[3]]]"
-- 3
--
-- >>> sumNums' "{\"a\":{\"b\":4},\"c\":-1}"
-- 3
--
-- {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
-- [] and {} both have a sum of 0.
-- You will not encounter any strings containing numbers.

sumNums' :: String -> Int
sumNums' = sum . fmap read . filter (/= "-") . extract start isNumber
  where start c = isNumber c || c == '-'

starOne' :: String -> String
starOne' = show . sumNums'
