{-# LANGUAGE OverloadedStrings #-}
module DayEleven where

import Data.Maybe(maybeToList, catMaybes)
import Data.List(delete, nub, nubBy, isInfixOf)
import Data.Vector(Vector)
import qualified Data.Vector as V
import System.IO.Unsafe(unsafePerformIO)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

solve :: IO Text
solve = do
  l <- TIO.readFile "inputs/eleven.input"
  let floors = parseInput (T.lines l)
  return . T.pack . show . solveStarOne . initialF $ floors

data Chip = M ID deriving (Eq, Ord, Show, Read)
data RTG  = G ID deriving (Eq, Ord, Show, Read)
type ID = Char -- Element
type Floor = ([Chip], [RTG])
type Slot = Either Chip RTG
type OSlot = Maybe Slot

type Floors = (Floor,Floor,Floor,Floor)
data Move = Mv {start :: (Int, Floor), end :: (Int, Floor), cargo :: (Slot, OSlot)} deriving (Show, Eq)
data Step = St {current :: Int, floors :: Floors} deriving (Show, Eq)

-- In process with current candidates, step counter, visited steps
-- or solve with counter
data SolutionStep = InProc [Step] Int [Step] | Solved Int deriving (Show)

-- | End game: when the fourth floor has everything.
--
endGame :: Step -> Bool
endGame (St _ (o, t, r, f)) = empty o && empty t && empty r && not (empty f)
  where empty ([], []) = True
        empty _        = False

-- |
-- >>> let mv = Mv (1, ([M 'H', M 'L'], [])) (2, ([], [G 'H'])) (Left (M 'H'), Nothing)
-- >>> takeStep testStep mv
-- St {current = 2, floors = (([M 'L'],[]),([M 'H'],[G 'H']),([],[G 'L']),([],[]))}
takeStep :: Step -> Move -> Step
takeStep (St _ fls) m@(Mv (i, f) (j, e) _) = St j (up i f' (up j e' fls))
  where
    (e', f') = makeMove m
    up :: Int -> Floor -> Floors -> Floors
    up 1 f (_, b, c, d) = (f, b, c, d)
    up 2 f (a, _, c, d) = (a, f, c, d)
    up 3 f (a, b, _, d) = (a, b, f, d)
    up 4 f (a, b, c, _) = (a, b, c, f)

currentFloor :: Step -> (Int, Floor)
currentFloor (St 1 (a, _, _, _)) = (1, a)
currentFloor (St 2 (_, b, _, _)) = (2, b)
currentFloor (St 3 (_, _, c, _)) = (3, c)
currentFloor (St 4 (_, _, _, d)) = (4, d)

floorAbove :: Step -> Maybe (Int, Floor)
floorAbove (St 1 (_, b, _, _)) = Just (2, b)
floorAbove (St 2 (_, _, c, _)) = Just (3, c)
floorAbove (St 3 (_, _, _, d)) = Just (4, d)
floorAbove (St 4 (_, _, _, _)) = Nothing

floorBelow :: Step -> Maybe (Int, Floor)
floorBelow (St 1 (_, _, _, _)) = Nothing
floorBelow (St 2 (a, _, _, _)) = Just (1, a)
floorBelow (St 3 (_, b, _, _)) = Just (2, b)
floorBelow (St 4 (_, _, c, _)) = Just (3, c)

nextFloors :: Step -> [(Int, Floor)]
nextFloors s = catMaybes $ [floorAbove, floorBelow] <*> pure s

-- |  A room is safe (no fried microchips) if there are no RTGS or if all chips are powered
--
-- >>> safe ([M 'H'], [G 'H'])
-- True
--
-- >>> safe ([M 'H'], [G 'L'])
-- False
--
-- >>> safe ([M 'H',M 'L'],[G 'H'])
-- False
--
-- >>> safe ([M 'H'],[G 'H',G 'L'])
-- True
--
safe :: Floor -> Bool
safe (chips, rtgs) = null rtgs || all powered chips
  where powered (M c) = elem (G c) rtgs

makeMove :: Move -> (Floor, Floor)
makeMove (Mv {start = (_, s), end = (_, e), cargo = c}) = (add e c, remove s c)

-- | Indicates whether a move is safe.
--
-- >>> safeMove $ Mv { start = (1,([M 'H',M 'L'],[])), end = (2,([],[G 'H'])), cargo = (Left (M 'H'), Nothing)}
-- True
--
-- >>> safeMove $ Mv { start = (1,([M 'H',M 'L'],[])), end = (2,([],[G 'H'])), cargo = (Left (M 'H'), Just . Left . M $ 'L')}
-- False
--
-- >>> safeMove $ Mv { start = (1,([M 'H',M 'L'],[])), end = (2,([],[G 'H', G 'L'])), cargo = (Left (M 'H'), Just . Left . M $ 'L')}
-- True
--
-- >>> safeMove $ Mv { start = (1,([M 'H',M 'L'],[])), end = (2,([],[G 'H', G 'L'])), cargo = (Left (M 'H'), Nothing) }
-- True
--
safeMove :: Move -> Bool
safeMove m = (\(a,b) -> safe a && safe b) $ makeMove m

enumSlots :: (Slot, OSlot) -> [Slot]
enumSlots (s, o) = s : maybeToList o

add :: Floor -> (Slot, OSlot) -> Floor
add f = foldr ins f . enumSlots
  where
    ins :: Slot -> Floor -> Floor
    ins (Left chip@(M _)) (chips, rtgs) = (chip:chips, rtgs)
    ins (Right rtg@(G _)) (chips, rtgs) = (chips, rtg:rtgs)


remove :: Floor -> (Slot, OSlot) -> Floor
remove f = foldr rm f . enumSlots
  where
    rm :: Slot -> Floor -> Floor
    rm (Left chip@(M _)) (chips, rtgs) = (delete chip chips, rtgs)
    rm (Right rtg@(G _)) (chips, rtgs) = (chips, delete rtg rtgs)


-- | generate next possible things to take from this floor
--
-- >>> nextCargos ([M 'H'],[])
-- [(Left (M 'H'),Nothing)]
--
-- >>> nextCargos ([M 'H'],[G 'H'])
-- [(Left (M 'H'),Nothing),(Left (M 'H'),Just (Right (G 'H'))),(Right (G 'H'),Nothing)]
--
-- >>> nextCargos ([M 'H'],[G 'H',G 'L'])
-- [(Left (M 'H'),Nothing),(Left (M 'H'),Just (Right (G 'H'))),(Left (M 'H'),Just (Right (G 'L'))),(Right (G 'H'),Nothing),(Right (G 'H'),Just (Right (G 'L'))),(Right (G 'L'),Nothing)]
--
nextCargos :: Floor -> [(Slot, OSlot)]
nextCargos (chips, rtgs) = nubBy (\a b -> enumSlots a `eq` enumSlots b) $
  [(Left c, o) :: (Slot, OSlot) | c <- chips, o <- (delete (Just (Left c)) os)] ++
  [(Right r, o) :: (Slot, OSlot)| r <- rtgs, o <- (delete (Just (Right r)) os)]
  where
    os :: [OSlot]
    os = Nothing : [Just (Left c) | c <- chips] ++ [Just (Right r) | r <- rtgs]
    eq :: Eq a => [a] -> [a] -> Bool
    eq as bs = length as == length bs && all ((flip elem) bs) as

nextMoves :: (Int, Floor) -> (Int, Floor) -> [Move]
nextMoves (i, e) f = fmap (Mv (i, e) f) (nextCargos e)

allNextMoves :: Step -> [Move]
allNextMoves s = concat $ fmap (nextMoves (currentFloor (s))) (nextFloors s)

testStep :: Step
testStep =
  let
    one = ([M 'H',M 'L'],[])
    two = ([],[G 'H'])
    thr = ([],[G 'L'])
  in St 1 (one,two,thr,([],[]))


-- throwing away unsafe steps, generate, from all current possible steps, next steps
--
iterateSolve :: SolutionStep -> SolutionStep
iterateSolve (InProc steps count history) =
  let nextSteps = filtered . nub . concat . fmap safeNexts $ steps
      safeNexts step = fmap (takeStep step) (safeMoves step)
      safeMoves = nub . filter safeMove . allNextMoves
      filtered = filter (not . (flip elem) history)
      nextCount = count + 1
      solved = any endGame nextSteps
  in
   if solved
   then Solved nextCount
   else InProc nextSteps nextCount (nub $ history ++ nextSteps)

initial :: Step -> SolutionStep
initial s = InProc [s] 0 []

initialF :: Floors -> SolutionStep
initialF = initial . St 1

parseF :: Text -> Floor
parseF t = (chips ws, gens ws)
  where
    ws = T.words t
    chips = catMaybes . fmap extractChip
    gens = catMaybes . fmap extractGen
    extractChip "thulium-compatible"    = Just (M 'T')
    extractChip "plutonium-compatible"  = Just (M 'L')
    extractChip "strontium-compatible"  = Just (M 'S')
    extractChip "promethium-compatible" = Just (M 'P')
    extractChip "ruthenium-compatible"  = Just (M 'R')
    extractChip _                       = Nothing
    extractGen "thulium"                = Just (G 'T')
    extractGen "plutonium"              = Just (G 'L')
    extractGen "strontium"              = Just (G 'S')
    extractGen "promethium"             = Just (G 'P')
    extractGen "ruthenium"              = Just (G 'R')
    extractGen _                        = Nothing

parseInput :: [Text] -> Floors
parseInput [o,t,h,f]
  | o `is` "first" &&
    t `is` "second" &&
    h `is` "third" &&
    f `is` "fourth" = (parseF o, parseF t, parseF h, parseF f)
  | otherwise = error "bad input"
  where is f t = (T.words f !! 1) == t

parseInput _ = error "bad input"

-- |
-- >>> solveStarOne (initial testStep)
-- 11
solveStarOne :: SolutionStep -> Int
solveStarOne i @ (InProc _ _ _) = solveStarOne (iterateSolve i)
solveStarOne (Solved i) = i
