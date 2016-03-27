module Thirteen where

import Data.List(maximum,permutations)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(fromJust,fromMaybe)

type Person = String
type HappinessEffect = (Person, Int)
type Preference = (Person, [HappinessEffect])
type Preferences = [Preference]
type Seating = [Person]

-- | Provides a list of each person along with their neighbours (the two on either side)
--
-- >>> neighbours ["a", "b", "c"]
-- [("a",("c","b")),("b",("a","c")),("c",("b","a"))]
--
neighbours :: Seating -> [(Person, (Person, Person))]
neighbours all = all `zip` (lefts `zip` rights)
  where lefts  = (last all) :  (init all)
        rights = (tail all) ++ [head all]

-- | Effect of a person sitting next to you, given your preferences.
--
-- >>> effectOf [("a", 1), ("b", 2)] "b"
-- 2
--
-- >>> effectOf [("Bob", 54),("Carol", -79),("David", -2)] "David"
-- -2
--
-- >>> effectOf [("Bob", 54),("Carol", -79),("David", -2)] "Bob"
-- 54
--
-- >>> effectOf [("Alice", 83),("Carol", -7),("David", -63)] "Alice"
-- 83
--
-- >>> effectOf [("Alice", 83),("Carol", -7),("David", -63)] "Carol"
-- -7
--
-- >>> effectOf [("Alice", -62),("Bob", 60),("David", 55)] "Bob"
-- 60
--
-- >>> effectOf [("Alice", -62),("Bob", 60),("David", 55)] "David"
-- 55
--
-- >>> effectOf [("Alice", 46),("Bob", -7),("Carol", 41)] "Carol"
-- 41
--
-- >>> effectOf [("Alice", 46),("Bob", -7),("Carol", 41)] "Alice"
-- 46
--
effectOf :: [HappinessEffect] -> Person -> Int
effectOf es p = fromMaybe 0 (lookup p es)

-- | Total happiness of a seating arrangement, given preferences.
--
-- >>> :{
-- let
--   aliceP = ("Alice",[("Bob", 54),("Carol", -79),("David", -2)])
--   bobP   = ("Bob",[("Alice", 83),("Carol", -7),("David", -63)])
--   carolP = ("Carol",[("Alice", -62),("Bob", 60),("David", 55)])
--   davidP = ("David",[("Alice", 46),("Bob", -7),("Carol", 41)])
--   preferences = aliceP:bobP:carolP:davidP:[]
-- in happiness preferences ["Alice","Bob","Carol","David"]
-- :}
-- 330
--
happiness :: Preferences -> Seating -> Int
happiness p s =
  let
    prefs = Map.fromList p
    neibs = neighbours s
    scores = do
      (q, (l, r)) <- neibs
      effect <- [effectOf (fromJust $ Map.lookup q prefs) l,
                 effectOf (fromJust $ Map.lookup q prefs) r]
      return effect
  in
   sum scores


-- | Find the optimal permutation of people to maximise happiness
--
-- >>> :{
-- let
--   aliceP = ("Alice",[("Bob", 54),("Carol", -79),("David", -2)])
--   bobP   = ("Bob",[("Alice", 83),("Carol", -7),("David", -63)])
--   carolP = ("Carol",[("Alice", -62),("Bob", 60),("David", 55)])
--   davidP = ("David",[("Alice", 46),("Bob", -7),("Carol", 41)])
--   preferences = aliceP:bobP:carolP:davidP:[]
-- in optimal ["Alice","Bob","Carol","David"] preferences
-- :}
-- 330
--
optimal :: Seating -> Preferences -> Int
optimal s p = maximum [happiness p seating | seating <- permutations s]

parse :: String -> (Seating, Preferences)
parse s =
  let
    parsedLines = fmap (\l -> parseWords (words l)) (lines s)
    preferences = Map.toList $ foldr (\(p,h) m -> Map.insertWith (++) p [h] m) Map.empty parsedLines
    seating = (fst . unzip) preferences
  in
   (seating, preferences)

parseWords :: [String] -> (Person, HappinessEffect)
parseWords (person:"would":gainLose:n:rest) = (person,(target,m))
  where
    m = if gainLose == "gain"
        then read n
        else - (read n)
    target = last rest
