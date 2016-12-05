module Four where

import Data.ByteString.Base16(encode)
import Data.ByteString.Char8(pack, unpack, ByteString)
import qualified Data.ByteString as BS
import Crypto.Hash.MD5(hash)

testNZeros :: Int -> String -> Bool
testNZeros n s = (take n s) == (take n (repeat '0'))

testHash :: String -> Bool
testHash = testNZeros 5

genHash :: String -> Int -> ByteString
genHash s i = (encode . hash . pack . concat) [s, show i]

findHashWith :: (String -> Bool) -> Int -> String -> Int
findHashWith f i s =
  let
    h = genHash s i
  in
    if f (unpack h)
    then i
    else findHashWith f (succ i) s


findHashN :: Int -> Int -> String -> Int
findHashN = findHashWith . testNZeros

findHash :: Int -> String -> Int
findHash = findHashN 5

findHarderHash :: Int -> String -> Int
findHarderHash = findHashN 6

starOne :: String -> String
starOne  = show . (findHash 1)

starTwo :: String -> String
starTwo  = show . (findHarderHash 1)
