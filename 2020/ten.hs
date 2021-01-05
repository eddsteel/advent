import Data.List (sort)

-- Ported https://github.com/migerh/aoc-2020-rs/blob/main/src/day10/mod.rs
differences :: [Int] -> [Int]
differences xs = dif <$> xs `zip` tail xs
  where dif (a, b) = b - a

combinations 2 = 2
combinations 3 = 4
combinations 4 = 7
combinations _ = 1

partition :: [Int] -> [Int]
partition [] = []
partition xs = length ones : (partition (dropWhile (== 3) rest))
  where (ones, rest) = span (== 1) xs

example :: [Int]
example = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
           38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

input :: IO [Int]
input = do
  ls <- fmap lines $ readFile "input/10"
  return $ map read ls

star_one :: [Int] -> Int
star_one input = ones * threes
  where chain = 0:(sorted ++ [last sorted + 3])
        sorted = sort input
        diffs = differences chain
        ones = length . filter (== 1) $ diffs
        threes = length chain - ones - 1

star_two :: [Int] -> Int
star_two input = let
                   chain = sort input
                   is = 0:chain ++ [(last chain) + 3]
                 in product . fmap combinations . partition . differences $ is

main :: IO ()
main = do
  is <- input
  putStr "Answer 1: "
  putStrLn (show $ star_one is)
  putStr "Answer 2: "
  putStrLn (show $ star_two is)

test :: IO ()
test = do
  putStr "Answer 1: "
  putStrLn (show $ star_one example)
  putStr "Answer 2: "
  putStrLn (show $ star_two example)

