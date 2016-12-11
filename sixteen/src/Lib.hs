module Lib
    ( currentDay
    ) where

import Data.Text(Text)
import qualified Data.Text as T
import qualified DayOne
import qualified DayTwo
import qualified DayThree

currentDay :: IO ()
currentDay = do
  result <- DayThree.solve
  putStrLn (T.unpack result)
