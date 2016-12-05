module Lib
    ( currentDay
    ) where

import Data.Text(Text)
import qualified Data.Text as T
import qualified DayOne

currentDay :: IO ()
currentDay = do
  result <- DayOne.solve
  putStrLn (T.unpack result)
