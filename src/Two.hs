module Two where

import Data.List(unfoldr)

data Box = Box Int Int Int
           deriving (Show, Read)

box :: String -> Box
box  = boxup . (unfoldr next)
  where
    next [] = Nothing
    next s = process $ span (/= 'x') s
    process (a, []) = Just (a, [])
    process (a, b) = Just (a, tail b)
    boxup [a, b, c] = Box (read a) (read b) (read c)
    boxup _ = error("wut")


areas            :: Box -> [Int]
areas (Box l w h) = [l * w, w * h, h * l]


surfaceArea :: Box -> Int
surfaceArea  = (*2) . sum . areas

smallestSide :: Box -> Int
smallestSide  = minimum .areas

paperNeeded   :: Box -> Int
paperNeeded b = area + extra
  where area = surfaceArea b
        extra = smallestSide b


smallestPerimeter :: Box -> Int
smallestPerimeter (Box l w h) = 2 * minimum [ l + w, w + h, h + l ]

volume :: Box -> Int
volume (Box l w h) = l * w * h

ribbonNeeded :: Box -> Int
ribbonNeeded b = ribbon + bow
  where
    ribbon = smallestPerimeter b
    bow = volume b


starOne :: String -> String
starOne = show . sum . (fmap (paperNeeded . box)) . lines

starTwo :: String -> String
starTwo = show . sum . (fmap (ribbonNeeded . box)) . lines
