module Lib
    ( currentDay
    ) where

import Data.Text(Text)
import qualified Data.Text as T
import qualified DayOne
import qualified DayTwo
import qualified DayThree
import qualified DayFour

currentDay :: IO ()
currentDay = do
  result <- DayFour.solve
  putStrLn (T.unpack result)
