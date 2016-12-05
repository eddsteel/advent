module Eleven where

import Data.Char(ord, chr)
import Data.List(insert, isPrefixOf, elem, nub, nubBy)
import Data.Map(Map, insertWith, toList, fromList)

type Password = String

-- | generate next password by "incrementing" supplied pw
--
-- >>> increment ""
-- "a"
-- >>> increment "z"
-- "aa"
-- >>> increment "abc"
-- "abd"
-- >>> increment "abcdefzz"
-- "abcdegaa"
increment :: Password -> Password
increment "" = "a"
increment s
  | last s == 'z' = mappend ((increment . init) s) "a"
  | otherwise = mappend (init s) [((inc . last) s)]

inc :: Char -> Char
inc = chr . (+ 1) . ord

-- | A string is an increasing straight if its characters increment by one step each.
-- >>> isStraight "abc"
-- True
-- >>> isStraight "kdo"
-- False
--
isStraight :: String -> Bool
isStraight as@(a:_) = as `isPrefixOf` (iterate inc a)


-- | A password has a straight of 3 if there is a n-length substring that is an increasing straight.
--
-- >>> hasStraight "hijklmmn"
-- True
hasStraight :: Password -> Bool
hasStraight s = any (\(a,b,c) -> isStraight [a,b,c]) $ zip3 s s' s''
  where s'  = tail s
        s'' = tail s'

-- | A password contains illegal characters if it contains 'i', 'o', or 'l'
--
-- >>> hasIllegalChars "hijklmmn"
-- True
-- >>> hasIllegalChars "great"
-- False
--
hasIllegalChars :: Password -> Bool
hasIllegalChars = any ((flip elem) "iol")


-- borrowed this from Five. They love their non-overlapping pairs.
grouped :: String -> [((Char, Char), [Int])]
grouped s =
    let
      pairsPos = (s `zip` tail s)`zip` [1..]
      grouped = foldr (\(a, n) b -> insertWith (++) a [n] b) (fromList []) pairsPos
    in toList grouped




-- | A password has two different pairs if it contains two *non-overlapping* pairs that are different.
--
-- >>> hasTwoDifferentPairs "abbceffg"
-- True
-- >>> hasTwoDifferentPairs "abbcegkl"
-- False
-- >>> hasTwoDifferentPairs "abbbcdef"
-- False
--
hasTwoDifferentPairs :: Password -> Bool
hasTwoDifferentPairs s =
  let
    g = grouped s
    h = filter (uncurry (==) . fst ) g
    dedupe = nubBy (\i j -> (abs (i - j)) <= 1)
    deduped = fmap (\ (cs, positions) -> (cs, dedupe positions)) h
    chars = nub $ fmap (fst . fst) deduped
  in
   length chars >= 2


-- | A password is valid if all three conditions are met
--
-- >>> isValid "hijklmmn"
-- False
-- >>> isValid "abbceffg"
-- False
-- >>> isValid "abcdffaa"
-- True
isValid :: Password -> Bool
isValid s = all id (fmap (apply s) conditions)
  where
    apply s' f = f s' -- presumably this has a name already
    conditions = [hasTwoDifferentPairs, hasStraight, not . hasIllegalChars]


-- | The next password from a supplied password is the next valid password reached by incrementing
--
-- >>> findNextPassword "abcdefgh"
-- "abcdffaa"
--
-- >>> findNextPassword "ghijklmn"
-- ghjaabcc
--
findNextPassword :: Password -> Password
findNextPassword = until isValid increment


starOne :: String
starOne = findNextPassword "vzbxkghb"

starTwo :: String
starTwo = findNextPassword (increment starOne)

main :: IO ()
main = putStrLn starOne
