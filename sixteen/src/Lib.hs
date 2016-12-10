module Lib
    ( currentDay
    ) where

import Data.Text(Text)
import qualified Data.Text as T
import qualified DayOne
import qualified DayTwo

currentDay :: IO ()
currentDay = do
  result <- DayTwo.solve
  putStrLn (T.unpack result)
