module One where

bite :: Char -> Int
bite '(' = 1
bite ')' = -1
bite _   = 0

biteAll  :: String -> Int
biteAll s = foldr (+) 0 $ fmap bite s


findBasement :: Int -> Int -> String -> Int
findBasement count (-1) _    = count
findBasement count f (c:cs)  = findBasement (count + 1) (adjust c) cs
  where adjust '(' = f + 1
        adjust ')' = f - 1


star1 :: String -> String
star1 = show . biteAll

star2 :: String -> String
star2 = show . (findBasement 0 0)
