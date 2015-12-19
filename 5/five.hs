module Five where

import Data.List(nubBy, nub)
import Data.Map(insertWith, Map, fromList, toList)

vowels :: String -> String
vowels = filter ((flip elem) "aeiou")

pairs :: [a] -> [(a, a)]
pairs s = s `zip` tail s

threeVowels :: String -> Bool
threeVowels = ((3 <=) . length . vowels)

oneDouble :: String -> Bool
oneDouble s = any (uncurry (==)) (pairs s)

noBadStrings :: String -> Bool
noBadStrings s = not . any ((flip elem) badPairs) $ pairs s
  where
    badPairs = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]

niceOne :: String -> Bool
niceOne s = threeVowels s && oneDouble s && noBadStrings s

starOne :: String -> String
starOne = show . length . filter niceOne . lines

strictPairs         :: [a] -> [(a, a)]
strictPairs []       = []
strictPairs [_]      = []
strictPairs (a:b:bs) = (a, b):(strictPairs bs)

twoIn :: Eq a => [(a, a)] -> Bool
twoIn s = length (nub s) /= length s

grouped :: String -> [((Char, Char), [Int])]
grouped s =
    let
      pairsPos = pairs s `zip` [1..]
      grouped = foldr (\(a, n) b -> insertWith (++) a [n] b) (fromList []) pairsPos
    in toList grouped


twoPair :: String -> Bool
twoPair s =
  let
    g = grouped s
    dedupe = nubBy (\i j -> (abs (i - j)) <= 1)
    deduped = fmap (\ (chars, positions) -> (chars, dedupe positions)) g
    winners = filter ((1 <) . length . snd) deduped
    pairs = fmap fst winners
  in
   length pairs >= 1

triples :: [a] -> [(a, a, a)]
triples s = zip3 s (tail s) (drop 2 s)

sammiches :: Eq a => [a] -> [(a, a, a)]
sammiches = (filter f) . triples
  where f (a,_,b) = a == b
        f _ = False

sammich :: String -> Bool
sammich  = (1 <=) . length . sammiches

niceTwo :: String -> Bool
niceTwo s = twoPair s && sammich s

starTwo :: String -> String
starTwo = show . length . filter niceTwo . lines


main :: IO ()
main = interact starTwo
