module Six where

import Data.List(foldl')
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

data Light = Light Int Int deriving (Eq, Show)
instance Ord Light where
  compare (Light a b) (Light c d) =
    let ac = compare a c
    in if ac == EQ then compare b d
       else ac

data Lights = Lights (Int, Int) (Int, Int) deriving (Show)

lights :: Lights -> Set Light
lights (Lights (a,b) (c,d)) = Set.fromList [Light x y | x <- [a..c], y <- [b..d]]

data Operation = TurnOn | TurnOff | Toggle deriving (Eq, Show)

type State = Set Light -- lights that are lit

-- | A rectangle of lights
-- This does not check you are providing coords in the right order.
-- Use `lights` to see them.
--
-- >>> lights $ (0, 0) `through` (2, 2)
-- fromList [Light 0 0,Light 0 1,Light 0 2,Light 1 0,Light 1 1,Light 1 2,Light 2 0,Light 2 1,Light 2 2]
--
through :: (Int, Int) -> (Int, Int) -> Lights
through ab cd = Lights ab cd


-- | Follow an instruction
--
-- >>> follow Set.empty (Toggle, (Lights (1, 1) (1, 1)))
-- fromList [Light 1 1]
--
-- >>> follow (follow Set.empty (Toggle, ((0,0) `through` (2,2)))) (TurnOff,((0,0) `through` (2,2))) == Set.empty
-- True
--
-- >>> follow Set.empty (TurnOff, ((0,0) `through` (2,2))) == Set.empty
-- True
--
follow :: State -> (Operation, Lights) -> State
follow s (TurnOn, l)  = Set.union s (lights l)
follow s (TurnOff, l) = Set.difference s (lights l)
follow s (Toggle, l)  = Set.union (lights l `Set.difference` s) (s `Set.difference` lights l)

parseCoords :: String -> (Int, Int)
parseCoords s = read $ "(" ++ s ++ ")"

parseLights :: [String] -> Lights
parseLights [ab,_,cd] = parseCoords ab `through` parseCoords cd
parseLights _ = error "bad input"


-- | Parse instructions from input
--
-- >>> parse "turn off 1,0 through 2,1"
-- (TurnOff,Lights (1,0) (2,1))
--
parse :: String -> (Operation, Lights)
parse s = case words s of
  ("toggle":rest) -> (Toggle, parseLights rest)
  ("turn":"on":rest) -> (TurnOn, parseLights rest)
  ("turn":"off":rest) -> (TurnOff, parseLights rest)
  _ -> error "bad input"

-- | Star One
--
-- >>> starOne "toggle 1,0 through 2,2\nturn off 1,1 through 2,2\nturn on 2,1 through 2,2" -- 1,0 2,0 2,1 2,2
-- "4"

starOne :: String -> String
starOne s = show . length $ foldl' (\a -> follow a . parse) Set.empty (lines s)


-- For star two we need to keep track of light levels
type State2 = Map Light Integer

inc :: Integer -> Light -> State2 -> State2
inc i l s = Map.insertWith (+) l i s

follow2 :: State2 -> (Operation, Lights) -> State2
follow2 os (TurnOn, l)  = foldr (inc 1) os (lights l)
follow2 os (TurnOff, l) = Map.filter (> 0) $ foldr (inc (-1)) os (lights l)
follow2 os (Toggle, l)  = foldr (inc 2) os (lights l)

starTwo :: String -> String
starTwo s = show . sum . (fmap snd) . Map.toList $ foldl' (\a -> follow2 a . parse) Map.empty (lines s)

main :: IO ()
main = interact starTwo
