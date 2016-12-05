module Three where

import Data.Set(insert, Set, fromList, size, union)
import Data.List(foldl')

type DChar = Char -- this could become a validated subset of Char

data Coords = Coords Int Int
            deriving (Show, Eq, Ord)

data Move = North
          | East
          | South
          | West
          deriving (Show, Eq)

data House = House Coords
           deriving (Show, Eq, Ord)

type Visits = Set House

type Moment = (Coords, Visits)

move :: DChar -> Move
move '>' = East
move '<' = West
move 'v' = South
move '^' = North

oneMove :: Coords -> Move -> Coords
oneMove (Coords x y) North = Coords (x + 1) y
oneMove (Coords x y) East  = Coords  x     (y + 1)
oneMove (Coords x y) South = Coords (x - 1) y
oneMove (Coords x y) West  = Coords  x     (y - 1)

step :: Moment -> Move -> Moment
step (coords, houses) move =
  let
    newLoc = oneMove coords move
    newCounts = insert (House newLoc) houses
  in (newLoc, newCounts)

startMoment :: Moment
startMoment = (Coords 0 0, fromList [House (Coords 0 0)])

journey :: [Move] -> Moment
journey = foldl' step startMoment

journeyS :: String -> Moment
journeyS = journey . (fmap move)

alternate :: [a] -> ([a], [a])
alternate [] = ([], [])
alternate (a:[]) = ([a], [])
alternate (a:b:rest) = (a:as, b:bs)
  where (as, bs) = alternate rest -- :(

fmap' :: (a -> b) -> (a, a) -> (b, b)
fmap' f (a, b) = (f a, f b)

roboJourney :: [Move] -> (Moment, Moment)
roboJourney ms = fmap' journey (alternate ms)

roboJourneyS :: String -> (Moment, Moment)
roboJourneyS = roboJourney . (fmap move)

starOne :: String -> String
starOne = show . size . snd . journeyS

starTwo :: String -> String
starTwo = show . size . (uncurry union) . (fmap' snd) . roboJourneyS
