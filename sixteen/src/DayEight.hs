{-# LANGUAGE OverloadedStrings #-}
module DayEight where

import Data.Text(Text)
import qualified Data.Text as T
import Data.Vector(Vector)
import Data.Vector((//),(!))
import qualified Data.Vector as V
import Data.Attoparsec.Text

solve :: IO String
solve = do
  instructions <- fmap (fmap parseInstruction . T.lines . T.pack) . readFile $ "inputs/eight.input"
  let result = foldl (flip exec) (blank 50 6) instructions
  return . unlines $ [solveStarOne result, solveStarTwo result]

solveStarOne :: Screen -> String
solveStarOne (S s) = show . V.sum . fmap V.sum . fmap (fmap count) $ s
  where count On = 1
        count Off = 0

solveStarTwo :: Screen -> String
solveStarTwo result= show result

type Horiz = Int
type Vert = Int
data Instruction = Rect Horiz Vert | RotateRow Vert Horiz | RotateColumn Horiz Vert
  deriving (Show, Eq)

data Pixel = On | Off deriving (Show, Read, Eq, Ord)
newtype Screen = S (Vector (Vector Pixel))

instance Show Screen where
  show (S s) = unlines . V.toList . fmap (V.toList . fmap pixelToChar) $ s

instance Read Screen where
  readsPrec _ s = [(readScreen s, "")]

-- |
-- >>> debug $ readScreen "...\n.#."
-- ...
-- .#.
--
readScreen :: String -> Screen
readScreen s = S $ (V.fromList . fmap (V.fromList . fmap charToPixel)) $ lines s

pixelToChar :: Pixel -> Char
pixelToChar On = '#'
pixelToChar Off = '.'

charToPixel :: Char -> Pixel
charToPixel '#' = On
charToPixel '.' = Off

off :: Int -> Pixel
off _ = Off

-- |
-- >>> debug $ blank 3 2
-- ...
-- ...
--
debug :: Show s => s -> IO ()
debug = putStr . show

fromLists :: [[Pixel]] -> Screen
fromLists = S . V.fromList . fmap (V.fromList)

blank :: Horiz -> Vert -> Screen
blank h v = fromLists $ fmap (\_ -> fmap off [1..h]) $ [1..v]

setPixel :: Pixel -> Horiz -> Vert -> Screen -> Screen
setPixel p x y (S s) = S $ s // [(y', (s ! y') // [(x', p)])]
  where (x', y') = bounds (S s) x y


bounds :: Screen -> Horiz -> Vert -> (Horiz, Vert)
bounds s x y = (x', y')
  where
    h = height s
    w = width s
    x' = x `mod` w
    y' = y `mod` h

-- |
-- >>> debug $ lightPixel 2 1 $ blank 3 3
-- ...
-- ..#
-- ...
--
lightPixel :: Horiz -> Vert -> Screen -> Screen
lightPixel = setPixel On

-- |
-- >>> debug $ clearPixel 0 1 $ read "#.\n#."
-- #.
-- ..
clearPixel :: Horiz -> Vert -> Screen -> Screen
clearPixel = setPixel Off

-- |
-- >>> getPixel 0 400 (blank 1000 1000)
-- Off
--
getPixel :: Horiz -> Vert -> Screen -> Pixel
getPixel x y (S s) = s ! y' ! x'
  where (x', y') = bounds (S s) x y

getPixels :: [(Horiz, Vert)] -> Screen -> [Pixel]
getPixels coords s = fmap (\(x,y) -> getPixel x y s) coords

-- |
-- >>> debug $ setPixels [(0,0), (0,2), (1,1), (2,0), (2,2)] [On,On,On,On,On] $ blank 3 3
-- #.#
-- .#.
-- #.#
setPixels :: [(Horiz, Vert)] -> [Pixel] -> Screen -> Screen
setPixels coords pixels s = foldl (\s ((x,y),p) -> setPixel p x y s) s (coords `zip` pixels)

-- |
-- >>> debug $ exec (Rect 3 2) $ blank 5 5
-- ###..
-- ###..
-- .....
-- .....
-- .....
--
-- >>> debug $ exec (parseInstruction "rect 3x2") $ blank 7 3
-- ###....
-- ###....
-- .......
--
-- >>> debug $ exec (parseInstruction "rotate column x=1 by 1") $ read "###....\n###....\n......."
-- #.#....
-- ###....
-- .#.....
--
-- >>> debug $ exec (parseInstruction "rotate row y=0 by 4") $ read "#.#....\n###....\n.#....."
-- ....#.#
-- ###....
-- .#.....
--
-- >>> debug $ exec (parseInstruction "rotate column x=1 by 1") $ read "....#.#\n###....\n.#....."
-- .#..#.#
-- #.#....
-- .#.....
--
exec :: Instruction -> Screen -> Screen
exec (Rect h v) s = foldl (\s (x, y) -> lightPixel x y s) s $ rectCoords 0 0 (h - 1) (v - 1)
exec (RotateColumn h v) s = setPixels (rotate v col) pixels s
  where
    ht = height s
    col = [(h, y) | y <- [0..(ht - 1)]]
    pixels = getPixels col s
exec (RotateRow v h) s = setPixels (rotate h row) pixels s
  where
    w = width s
    row = [(x, v) | x <- [0..(w - 1)]]
    pixels = getPixels row s

-- |
-- >>> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
rotate :: Int -> [a] -> [a]
rotate i as = Prelude.take (length as) (drop i (cycle as))

height :: Screen -> Vert
height (S s) = length s

width :: Screen -> Vert
width (S s) = length (s ! 0)

rectCoords :: Horiz -> Vert -> Horiz -> Vert -> [(Horiz, Vert)]
rectCoords h1 v1 h2 v2 = [(x,y) | x <- [h1..h2], y <- [v1..v2]]


-- do rotates in terms of lists of pixels to swap

rectP :: Parser Instruction
rectP = do
  h <- string "rect " *> decimal
  v <- string "x" *> decimal
  return $ Rect h v

rrowP :: Parser Instruction
rrowP = do
  vrt <- string "rotate row y=" *> decimal
  hrz <- string " by " *> decimal
  return $ RotateRow vrt hrz

rcolP :: Parser Instruction
rcolP = do
  hrz <- string "rotate column x=" *> decimal
  vrt <- string " by " *> decimal
  return $ RotateColumn hrz vrt

-- |
-- >>> parseInstruction "rect 1x1"
-- Rect 1 1
-- >>> parseInstruction "rotate row y=0 by 2"
-- RotateRow 0 2
-- >>> parseInstruction "rotate column x=32 by 1"
-- RotateColumn 32 1
--
parseInstruction :: Text -> Instruction -- no bad input
parseInstruction t = bail $ parseOnly (choice [rectP, rrowP, rcolP]) t

bail :: Either String Instruction -> Instruction
bail (Left s) = error s
bail (Right i) = i
