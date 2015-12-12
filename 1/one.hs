module One where

bite :: Char -> Int
bite '(' = 1
bite ')' = -1
bite _   = 0

biteAll  :: String -> Int
biteAll s = foldr (+) 0 $ fmap bite s

star1 = interact $ show . biteAll


star2 = interact $ show . (findBasement 0 0)

findBasement :: Int -> Int -> String -> Int
findBasement count (-1) _    = count
findBasement count f (c:cs)  = findBasement (count + 1) (adjust c) cs
  where adjust '(' = f + 1
        adjust ')' = f - 1


main = star2
