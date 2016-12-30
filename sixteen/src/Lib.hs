module Lib (currentDay) where

import Data.Text(Text)
import qualified Data.Text as T
import qualified DayOne
import qualified DayTwo
import qualified DayThree
import qualified DayFour
import qualified DayFive
import qualified DaySix
import qualified DaySeven
import qualified DayEight
import qualified DayNine

currentDay :: IO ()
currentDay = DayNine.solve >>= putStrLn . T.unpack
