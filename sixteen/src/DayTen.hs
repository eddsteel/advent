{-# LANGUAGE OverloadedStrings #-}
module DayTen where

import System.IO.Unsafe(unsafePerformIO)

import Control.Applicative((<|>))
import Data.Text(Text)
import qualified Data.Text as T
import Data.Maybe(isJust, catMaybes)
import Data.Map(Map)
import Data.List(foldl')
import qualified Data.Map as M
import Data.Vector(Vector, (//), (!))
import qualified Data.Vector as V
import Data.Attoparsec.Text hiding (take)
import qualified Data.Text.IO as TIO

solve :: IO Text
solve = do
  lines <- T.lines <$> TIO.readFile "inputs/ten.input"
  let instructions = parseInstruction <$> lines
  let state = runInstructions instructions
  let one = solveStarOne state
  let two = solveStarTwo state
  return . T.unlines $ [one, two]

solveStarOne :: State -> Text
solveStarOne = T.pack . show . botFor 61 17

solveStarTwo :: State -> Text
solveStarTwo = T.pack . show . product . catMaybes . fmap value . V.toList . V.take 3 . outputs
  where value (Out x) = x

data Bot = Bot {botId :: BotID, instruction :: Maybe Move, chips :: Chips} deriving (Show)
data Node = B BotID | O Int deriving (Show)
data Move = Give Node Node deriving (Show) -- give low, give high
data Instruction = Instruct BotID Move | Assign BotID Chip deriving (Show)
type State = (Vector Bot, Vector Output, Map Chips BotID)
type Chips = (Maybe Chip, Maybe Chip)
type Chip = Int -- Chip's value
type BotID = Int
newtype Output = Out (Maybe Chip) deriving (Show)

class Recipient a where -- things that can be given chips
  receive :: a -> Chip -> a

instance Recipient Bot where
  receive b chip = Bot { botId = botId b, instruction = instruction b, chips = addChip chip (chips b) }

instance Recipient Output where
  receive (Out Nothing) chip = Out $ Just chip

addChip :: Chip -> Chips -> Chips
addChip c (Nothing, Nothing) = (Just c, Nothing)
addChip c (Nothing, Just d)
  | d >= c = (Just c, Just d)
  | otherwise = (Just d, Just c)
addChip c (Just d, Nothing)
  | d >= c = (Just c, Just d)
  | otherwise = (Just d, Just c)
addChip _ _ = error "Trying to add a chip to a full complement"

bid :: Instruction -> BotID
bid (Instruct b _) = b
bid (Assign b _)   = b


run :: Bot -> State -> State
run b s = case instruction b of
  Just i -> let cs = (chips b)
            in updateComparisons (botId b) cs $ updateBot (runMove i cs s) (noChips b)
  Nothing -> s

updateState :: Node -> Chip -> State -> State
updateState (O o) c (bs, os, cs) = (bs, updateReceiver os o c, cs)
updateState (B b) c (bs, os, cs) = (updateReceiver bs b c, os, cs)

updateReceiver :: Recipient r => Vector r -> Int -> Chip -> Vector r
updateReceiver rs i c = rs // [(i, receive (rs ! i) c)]

updateComparisons :: BotID -> Chips -> State -> State
updateComparisons bid chips (bs, os, cs) = (bs, os, cs')
  where cs' = M.insert chips bid cs

-- |
-- >>> take 2 . V.toList . (\(_,c,_) -> c) $ runMove (Give (O 0) (O 1)) (Just 1, Just 2) empty
-- [Out (Just 1),Out (Just 2)]
--
runMove :: Move -> Chips -> State -> State
runMove _ (Nothing, _) _ = error "bot's not ready"
runMove _ (_, Nothing) _ = error "bot's not ready"
runMove (Give a b) (Just low, Just high) s =
  updateState a low (updateState b high s)

empty :: State
empty =
  (V.fromList [Bot {botId = bid, instruction = Nothing, chips = (Nothing, Nothing)} | bid <- [0..300]],
   V.replicate 100 $ Out Nothing,
   M.fromList [])

instructBot :: Instruction -> Bot -> Bot
instructBot (Assign bid chip) b
  | botId b == bid = receive b chip
  | otherwise = error $ "Giving instruction to wrong bot " ++ show bid ++ " / " ++ show (botId b)
instructBot (Instruct bid mv) b
  | botId b == bid = Bot {botId = botId b, instruction = Just mv, chips = chips b}
  | otherwise = error $ "Giving instruction to wrong bot " ++ show bid ++ " / " ++ show (botId b)

isReady :: Bot -> Bool
isReady b = isJust (instruction b) && defined (chips b)
  where defined (Just _, Just _) = True
        defined _ = False

withBot :: BotID -> State -> (Bot -> a) -> a
withBot i s f = f $ fst3 s ! i

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

updateBot :: State -> Bot -> State
updateBot (bs, os, cs) bot = (bs', os, cs)
  where bs' = bs // [(botId bot, bot)]

step :: State -> Instruction -> State
step s i = withBot (bid i) s $ \b ->
                        let b' = instructBot i b
                            s' = updateBot s b'
                        in
                          stepBot s' b'

noChips :: Bot -> Bot
noChips b = Bot {botId = botId b, instruction = instruction b, chips = (Nothing, Nothing)}

-- Run a bot change, but then see if that causes the state to have another
-- ready bot, since the execution of this seems to be expected to be eager
stepBot :: State -> Bot -> State
stepBot s b =
  let s'   = if isReady b then run b s else s
      next = V.find isReady (fst3 s')
  in case next of
       Nothing -> s'
       Just b -> stepBot s' b

outputs :: State -> Vector Output
outputs (_, os, _) = os

botFor :: Chip -> Chip -> State -> Maybe BotID
botFor i j (_, _, cs) = M.lookup chips cs
  where chips = addChip i . addChip j $ (Nothing, Nothing)

-- |
-- >>> putStr . unlines $ [show . V.take 3 . outputs, show . botFor 2 5] <*> [runInstructions [Assign 2 5,Instruct 2 (Give (B 1) (B 0)),Assign 1 3,Instruct 1 (Give (O 1) (B 0)),Instruct 0 (Give (O 2) (O 0)),Assign 2 2]]
-- [Out (Just 5),Out (Just 2),Out (Just 3)]
-- Just 2
--
runInstructions :: [Instruction] -> State
runInstructions is = foldl' step empty is

nodeP :: Parser Node
nodeP = (O <$> ("output " *> decimal)) <|>
        (B <$> ("bot " *> decimal))

moveP :: Parser Move
moveP = do
  n1 <- "low to " *> nodeP
  n2 <- " and high to " *> nodeP
  return $ Give n1 n2

instructP :: Parser Instruction
instructP = do
  id <- "bot " *> decimal <* " gives "
  mv <- moveP
  return $ Instruct id mv

assignP :: Parser Instruction
assignP = do
  id <- "value " *> decimal
  bt <- " goes to bot " *> decimal
  return $ Assign bt id

instructionP :: Parser Instruction
instructionP = choice [instructP, assignP]

parseInstruction :: Text -> Instruction
parseInstruction t = case parseOnly instructionP t of
  Left err -> error err
  Right i -> i
