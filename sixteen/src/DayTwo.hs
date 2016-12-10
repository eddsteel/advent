{-# LANGUAGE OverloadedStrings #-}
module DayTwo
where

import Data.Char(intToDigit, toUpper, digitToInt)
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import qualified Data.Text as T

-- | $setup
-- >>> :set -XOverloadedStrings

solve :: IO Text
solve = do
  lines <- readInputLines
  let resultOne = runLines startOne lines
  let resultTwo = runLines startTwo lines
  line1 <- (return . T.concat . (fmap text)) resultOne
  line2 <- (return . T.pack) resultTwo
  return $ T.concat [line1, "\n", line2]

startOne :: Digit
startOne = 5

startTwo :: Char
startTwo = '5'

readInputLines :: IO [Text]
readInputLines = do
  t <- readFile "inputs/two.input"
  (return . T.lines . T.pack) t

-- |
-- >>> runLines (5 :: Digit) ["ULL", "RRDDD", "LURDL", "UUUUD"] -- star 1 keypad
-- [1,9,8,5]
-- >>>  runLines '5' ["ULL", "RRDDD", "LURDL", "UUUUD"] -- star 2 keypad
-- "5DB3"
runLines :: Keypad k => k -> [Text] -> [k]
runLines k = reverse . snd . foldl step (k, [])
  where
    step (d, ds) t = let next = runLine d (readLine t)
                     in (next, next:ds)

-- |
-- >>> runLine (5 :: Digit) $ readLine "ULL"
-- 1
--
-- >>> scanl (flip move) 'D' $ readLine "LURDL"
-- "DDBCCB"
-- >>> runLine 'D' $ readLine "LURDL"
-- 'B'
runLine :: Keypad k => k -> Line -> k
runLine = foldl (flip move)

readLine :: Text -> Line
readLine t = read . T.unpack <$> T.chunksOf 1 t

data Move = U | D | L | R deriving (Enum, Eq, Ord, Show, Read)
type Line = [Move]
type Pos = (Int, Int)

class Keypad k where
  pos :: k -> Pos
  digit :: Pos -> Maybe k

-- | move according to instruction from starting point
-- >>> move U (5 :: Digit)
-- 2
-- >>> move L (2 :: Digit)
-- 1
-- >>> move L (1 :: Digit)
-- 1
-- >>> move L (3 :: Digit)
-- 2
-- >>> move L (4 :: Digit)
-- 4
-- >>> move L (5 :: Digit)
-- 4
-- >>> move L (6 :: Digit)
-- 5
-- >>> move U $ move U (9 :: Digit)
-- 3
-- >>> move U (1 :: Digit)
-- 1
-- >>> move R 'B'
-- 'C'
-- >>> move L 'C'
-- 'B'
--
-- >>> T.concat $ fmap (\(m,d)-> text $ move m (d :: Digit)) [(m,d) | m <- [U,L,D,R], d <- [1..9]]
-- "123123456112445778456789789233566899"
move :: Keypad k => Move -> k -> k
move m d =
  let
    move (x, y) =
        case m of
          U -> (x, y + 1)
          L -> (x - 1, y)
          D -> (x, y - 1)
          R -> (x + 1, y)
    moveOrStay f p = fromMaybe d (digit (f p))
  in
    moveOrStay move (pos d)

-- | digit to text
-- >>> text 1
-- "1"
text :: Show a => a -> Text
text = T.pack . show

-- | Star 1
-- 123
-- 456
-- 789
-- >>> digit (0,0) :: Maybe Digit
-- Just 7
-- >>> digit (2,2) :: Maybe Digit
-- Just 3
-- >>> digit (1,2) :: Maybe Digit
-- Just 2
-- >>> pos (2 :: Digit)
-- (1,2)
-- >>> pos (8 :: Digit)
-- (1,0)
-- >>> pos (5 :: Digit)
-- (1,1)
type Digit = Int

instance Keypad Int where
  digit (x, y)
    | x < 0 || y < 0 = Nothing
    | x > 2 || y > 2 = Nothing
    | otherwise = Just $ 9 - (2 - x) - 3 * y
  pos d =
    ((d - 1) `rem` 3, 2 - ((d - 1) `div` 3))


-- | Star 2
--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
--
-- >>> digit (1,1) :: Maybe Char
-- Just 'A'
-- >>> digit (2,2) :: Maybe Char
-- Just '7'
-- >>> digit (1,3) :: Maybe Char
-- Just '2'
-- >>> digit (3,1) :: Maybe Char
-- Just 'C'
-- >>> pos 'C'
-- (3,1)
-- >>> pos 'B'
-- (2,1)
--
type Hex = Char

hex :: Int -> Maybe Char
hex n
  | n > 0 && n < 14 = (Just . toUpper . intToDigit) n
  | otherwise = Nothing

-- >>> digit (0,0) :: Hex
-- Nothing
-- >>> digit (2, 2)
-- Just '7'
-- >>> pos '7'
-- (2, 2)
-- >>> pos '9'
-- (4, 2)
instance Keypad Char where
  digit (2, 0) = Just 'D'
  digit (0, 2) = Just '5'
  digit (2, 4) = Just '1'
  digit (4, 2) = Just '9'
  digit (x, y) -- [(x, y)| x <- [1..3], y <- [1..3]] is defined
    | x > 0 && x < 4 && y > 0 && y < 4 = hex $ 13 + x - (4 * y)
    | otherwise = Nothing
  pos '1' = (2, 4)
  pos '5' = (0, 2)
  pos '9' = (4, 2)
  pos 'D' = (2, 0)
  pos c = (x c, y c)
    where x c = rem ((digitToInt c) - 1) 4
          y c = 3 - (div ((digitToInt c) - 1) 4)
