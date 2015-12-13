module Main where

import Four


starOne :: String -> String
starOne  = show . (findHash 1)

starTwo :: String -> String
starTwo  = show . (findHarderHash 1)

main :: IO ()
main = interact starOne
