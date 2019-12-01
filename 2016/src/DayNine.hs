{-# LANGUAGE OverloadedStrings #-}
module DayNine where

import Prelude hiding (take)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text


class Decipherable a where
  declen :: a -> Int -- deciphered length

data Marked = Lit Text | Markr Int [Marked] deriving (Show, Read)

-- |
-- >>> declen $ Markr 3 [Lit "XYZ"]
-- 9
--
-- >>> declen $ [Lit "X",Markr 2 [Markr 3 [Lit "ABC"]],Lit "Y"]
-- 20
--
-- >>> declen $ [Markr 12 [Markr 12 [Markr 14 [Markr 10 [Markr 12 [Lit "A"]]]]]]
-- 241920
--
-- (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN -> 445
-- >>> declen $ [Markr 3 [Markr 3 [Lit "ABC"],Markr 3 [Lit "XY"],Markr 2 [Lit "PQRST"]],Lit "X",Markr 9 [Markr 2 [Lit "TWO"],Markr 7 [Lit "SEVEN"]]]
-- 445
instance Decipherable Marked where
  declen (Lit s) = T.length s
  declen (Markr r ms) = r * declen ms

instance Decipherable a => Decipherable [a] where
  declen = sum . fmap declen

solve :: IO Text
solve = do
  t <- TIO.readFile "inputs/nine.input"
  return $ T.unlines [solveStarOne t, solveStarTwo t]

solveStarOne :: Text -> Text
solveStarOne = T.pack . show . declen . parseInput1

-- for star 1, only one level deep
-- |
-- >>> parseOnly markerP1 "(1x5)B"
-- Right (Markr 5 [Lit "B"])
markerP1 :: Parser Marked
markerP1 = do
  c <- "(" *> decimal
  r <- "x" *> decimal <* ")"
  op <- take c
  return $ Markr r [Lit op]

litP :: Parser Marked
litP = Lit <$> takeWhile1 (/= '(')

-- for star 1, only one level deep
--
markedsP1 :: Parser [Marked]
markedsP1 = many1' (choice [markerP1, litP])

-- |
-- >>> declen . parseInput1 $ "ADVENT"
-- 6
--
-- >>> declen . parseInput1 $ "A(1x5)BC"
-- 7
--
-- >>> declen . parseInput1 $ "(3x3)XYZ"
-- 9
--
-- >>> declen . parseInput1 $ "A(2x2)BCD(2x2)EFG"
-- 11
--
-- >>> declen . parseInput1 $ "(6x1)(1x3)A"
-- 6
--
-- >>> declen . parseInput1 $ "X(8x2)(3x3)ABCY"
-- 18
--
parseInput1 :: Text -> [Marked]
parseInput1 t = either error id $ parseOnly markedsP1 t


solveStarTwo :: Text -> Text
solveStarTwo = T.pack . show . declen . parseInput2


markerP2 :: Parser Marked
markerP2 = do
  c <- "(" *> decimal
  r <- "x" *> decimal <* ")"
  cs <- take c
  let ms = parseInput2 cs
  return $ Markr r ms


-- |
-- >>> parseInput2 "X(8x2)(3x3)ABCY"
-- [Lit "X",Markr 2 [Markr 3 [Lit "ABC"]],Lit "Y"]
--
-- >>> parseInput2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
-- [Markr 3 [Markr 3 [Lit "ABC"],Markr 3 [Lit "XY"],Markr 2 [Lit "PQRST"]],Lit "X",Markr 9 [Markr 2 [Lit "TWO"],Markr 7 [Lit "SEVEN"]]]
markedsP2 :: Parser [Marked]
markedsP2 = many1' (choice [markerP2, litP])

parseInput2 :: Text -> [Marked]
parseInput2 t = either error id $ parseOnly markedsP2 t
