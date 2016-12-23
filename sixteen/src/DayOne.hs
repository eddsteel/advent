{-# LANGUAGE OverloadedStrings #-}
module DayOne
where

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Set(Set)
import qualified Data.Set as S

solve :: IO Text
solve = do
  name <- return "Day One:"
  o <- solveOne
  t <- solveTwo
  return $ T.concat [name, "\n  ", o, "\n  ", t]

solveOne :: IO Text
solveOne = do
  instructions <- readDomain "one"
  let result = run start instructions
  showAnswer (distance . loc) result

solveTwo :: IO Text
solveTwo = do
  instructions <- readDomain "one"
  let result = firstRevisit start (S.fromList []) instructions
  showAnswer distance result

-- some candidates for extracting for other days
readWords :: Text -> IO [Text]
readWords t = do
  ws <- TIO.readFile (T.unpack t)
  let ws'= stripCommas ws
  return (T.words ws')

readDomain :: (Read a) => Text -> IO [a]
readDomain t = fmap (read . T.unpack) <$> readWords (T.concat ["inputs/", t, ".input"])

showAnswer :: (Show a) => (b -> a) -> b -> IO Text
showAnswer f v = (return . T.pack . show . f) v
-- end

data Direction = L | R deriving (Eq, Ord)
instance Show Direction where
  show L = "L"
  show R = "R"

data Instruction = Instruction Direction Int deriving (Eq, Ord)
instance Show Instruction where
  show (Instruction d i) = show d ++ show i

instance Read Instruction where
  readsPrec _ s = readInstruction s

-- | read an instruction from String
--
-- >>> readInstruction "L1"
-- [(L1,"")]
--
-- >>> readInstruction "R186"
-- [(R186,"")]
--
readInstruction           :: String -> [(Instruction, String)]
readInstruction ('L':rest) = [(Instruction L (read rest),"")]
readInstruction ('R':rest) = [(Instruction R (read rest), "")]
readInstruction _          = []

-- | remove commas
-- >>> stripCommas $ Data.Text.pack "L1, R2, L7"
-- "L1 R2 L7"
--
stripCommas :: Text -> Text
stripCommas = T.filter (/= ',')

data Location = Loc Int Int deriving (Eq, Ord, Show, Read)
data Orientation = North | East | South | West deriving (Enum, Eq, Ord, Show, Read)

data Position = Pos Location Orientation deriving (Show, Eq, Ord)

position :: Location -> Orientation -> Position
position l p = Pos l p

-- | Turn left or right
-- >>> turn L North
-- West
-- >>> turn R $ turn R West
-- East
--
turn  :: Direction -> Orientation -> Orientation
turn L North = West
turn R West  = North
turn L o     = pred o
turn R o     = succ o

-- | Make a move, returning all locations passed through
-- >>> move North 5 (Loc 0 0)
-- [Loc 0 1,Loc 0 2,Loc 0 3,Loc 0 4,Loc 0 5]
-- >>> move West 3 (Loc 7 2)
-- [Loc 6 2,Loc 5 2,Loc 4 2]
--
move                  :: Orientation -> Int -> Location -> [Location]
move North i (Loc x y) = [Loc x (y + j) | j <- [1..i]]
move East i (Loc x y)  = [Loc (x + j) y | j <- [1..i]]
move South i (Loc x y) = [Loc x (y - j) | j <- [1..i]]
move West i (Loc x y)  = [Loc (x - j) y | j <- [1..i]]


-- | calculate distance from origin
-- >>> distance (Loc 2 3)
-- 5
-- >>> distance (Loc 0 (-2))
-- 2
-- >>> distance (Loc 10 2)
-- 12
--
distance :: Location -> Int
distance (Loc x y) = abs x + abs y

start :: Position
start = Pos (Loc 0 0) North

-- | Run a single instruction, providing all positions passed through.
step :: Position -> Instruction -> [Position]
step (Pos l o) (Instruction t i) =
  let
    o' = turn t o
    ls' = move o' i l
  in
   fmap ((flip position) o') ls'

loc :: Position -> Location
loc (Pos l _) = l

-- | Run a series of instructions, ending up in a position
--
-- >>> run start (fmap read ["R2", "L3"])
-- Pos (Loc 2 3) North
-- >>> run start (fmap read ["R2", "R2", "R2"])
-- Pos (Loc 0 (-2)) West
-- >>> run start (fmap read ["R5", "L5", "R5", "R3"])
-- Pos (Loc 10 2) South
--
run :: Position -> [Instruction] -> Position
run start is = foldl (\p i -> last (step p i)) start is


-- | Run a series of instructions, finding the first time a location is revisited.
--
-- >>> firstRevisit start (Data.Set.fromList []) (fmap read ["R8", "R4", "R4", "R8"])
-- Loc 4 0
--
firstRevisit :: Position -> Set Location -> [Instruction] -> Location
firstRevisit p@(Pos l ort) visits (i:is) =
  let
    steps = step p i
    locations = fmap loc steps
    matching = filter ((flip S.member) visits) locations
  in
    if null matching
    then firstRevisit (last steps) (S.union (S.fromList locations) visits) is
    else head matching
