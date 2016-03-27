module Seven(starOne) where

import Data.List(foldl')
import Data.Char(isNumber)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Bits
import Data.Maybe(fromJust)
import Data.MemoUgly(memo)

import System.IO.Unsafe(unsafePerformIO)


type Label = String

type Value = Int

type State = Map Label Expr

data Connect = Connect Expr Label deriving (Show)

data LabelOrValue = L Label | V Value deriving (Show, Ord, Eq)

data Expr = And LabelOrValue LabelOrValue
          | Or LabelOrValue LabelOrValue
          | LShift LabelOrValue Int
          | RShift LabelOrValue Int
          | Not LabelOrValue
          | LV LabelOrValue
          deriving (Show, Ord, Eq)


-- | eval an expr
--
-- >>> eval (Map.fromList [("a",(LV (V 1)))]) (L "a")
-- Just 1
--
-- >>> eval (Map.fromList [("a",(LV (L "c"))),("c",(LV (V 3)))]) (L "a")
-- Just 3
--
-- >>> eval (Map.fromList [("a",(LV (V 4))),("b",(LV (V 6))),("c",And (L "a") (L "b"))]) (L "c")
-- Just 4
--
eval :: State -> Expr -> Maybe Value
eval s (And a b)    =
  let
    _ = (unsafePerformIO . putStrLn . show) (And a b)
  in
    (.&.) <$> eval s (LV a) <*> eval s (LV b)
eval s (Or a b)     =
  let
    _ = (unsafePerformIO . putStrLn . show)(Or a b)
  in
    (.|.) <$> eval s (LV a) <*> eval s (LV b)
eval s (LShift a i) =
  let
    _ = (unsafePerformIO . putStrLn . show) (LShift a i)
  in
    (flip shift) i <$> eval s (LV a)
eval s (RShift a i) =
  let
    _ = (unsafePerformIO . putStrLn . show) (RShift a i)
  in
   (flip shift) (-i) <$> eval s (LV a)
eval s (Not a) =
  let
    _ = (unsafePerformIO . putStrLn . show) (Not a)
  in
    complement <$> eval s (LV a)
eval s (LV (L l)) =
  let
    _ = (unsafePerformIO . putStrLn . show) (LV (L l))
  in
    Map.lookup l s >>= eval s
eval _ (LV (V v))   =
  let
    _ = (unsafePerformIO . putStrLn . show) (V v)
  in
    Just v


meval = curry $ memo (uncurry eval)

insert :: State -> Connect -> State
insert s (Connect e l) = Map.insert l e s


parseLV :: String -> LabelOrValue
parseLV s
 | all isNumber s = V (read s)
 | otherwise      = L s


-- | Parse a string to a connect command
--
-- >>> parse "123 -> x"
-- Just (Connect (LV (V 123)) "x")
-- >>> parse "456 -> y"
-- Just (Connect (LV (V 456)) "y")
-- >>> parse "x AND y -> d"
-- Just (Connect (And (L "x") (L "y")) "d")
-- >>> parse "x OR y -> e"
-- Just (Connect (Or (L "x") (L "y")) "e")
-- >>> parse "x LSHIFT 2 -> f"
-- Just (Connect (LShift (L "x") 2) "f")
-- >>> parse "y RSHIFT 2 -> g"
-- Just (Connect (RShift (L "y") 2) "g")
-- >>> parse "NOT x -> h"
-- Just (Connect (Not (L "x")) "h")
-- >>> parse "NOT y -> i"
-- Just (Connect (Not (L "y")) "i")
--
parse :: String -> Maybe Connect
parse s = case (words s) of
  x:"->":l:[]            -> Just $ Connect (LV  (parseLV x)) l
  x:"AND":y:"->":l:[]    -> Just $ Connect (And (parseLV x) (parseLV y)) l
  x:"OR":y:"->":l:[]     -> Just $ Connect (Or  (parseLV x) (parseLV y)) l
  x:"LSHIFT":n:"->":l:[]
    | (all isNumber n)   -> Just $ Connect (LShift (parseLV x) (read n)) l
  x:"RSHIFT":n:"->":l:[]
    | (all isNumber n)   -> Just $ Connect (RShift (parseLV x) (read n)) l
  "NOT":x:"->":l:[]      -> Just $ Connect (Not (parseLV x)) l
  _                      -> Nothing


-- | Run commands given as a string, lookup the value of a
--
-- starOne "123 -> x\n456 -> y\nx AND y -> a"
-- "72"
--
starOne :: String -> String
starOne s = show $ do
  m <- build (lines s)
  let _ = unsafePerformIO $ putStrLn (show m)
  let _ =  unsafePerformIO $ putStrLn "Built the state"
  return $ meval m (LV (L "a"))

build :: [String] -> Maybe State
build = foldr step (Just Map.empty)
  where
    step l (Just m) =
      let _ = unsafePerformIO $ putStr "."
      in insert m <$> parse l
    step l Nothing  = error("bailing on " ++ l)
