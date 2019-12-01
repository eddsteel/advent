{-# LANGUAGE OverloadedStrings #-}
module DayFive where

import Prelude hiding (concat, drop)
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List(sortOn)
import Data.Maybe(catMaybes)
import Data.Char(isDigit)
import Data.Int(Int64)
import Data.Map.Lazy

md5sum :: B.ByteString -> B.ByteString
md5sum = B.pack . show . md5


-- |
-- >>> digit 6 "abc3231929"
-- "1"
-- >>> digit 6 "abc5017308"
-- "8"
-- >>> digit 6 "abc5278568"
-- "f"
digit :: Int64 -> B.ByteString -> B.ByteString
digit i = B.take 1 . B.drop (i - 1) . md5sum

-- |
-- >>> hit $ md5sum "abc3231929"
-- True
hit :: B.ByteString -> Bool
hit = B.isPrefixOf "00000"


hashInput :: B.ByteString -> Int -> B.ByteString
hashInput bs i = B.concat [bs, B.pack (show i)]

-- |
-- We do i + 1 not i, so that iterate works
-- >> > interestingNumber "abc" 1
-- 3231929
interestingNumber :: B.ByteString -> Int -> Int
interestingNumber bs i =
  if hit (md5sum (hashInput bs (i + 1)))
  then i + 1
  else interestingNumber bs (i + 1)

input :: B.ByteString
input = "reyedfim"

ints :: B.ByteString -> [Int]
ints input = tail $ (Prelude.iterate (interestingNumber input) 1)

-- |
-- > >> solve1 "abc"
-- "18f47a30"
--
solve1 :: B.ByteString -> B.ByteString
solve1 bs = B.concat $ Prelude.take 8 $ fmap (digit 6 . hashInput bs) (ints bs)

-- |
-- >>> starOneChar "abc" 3231929
-- Just (1,"5")
-- >>> starOneChar "abc" 5017308
-- Nothing
starOneChar :: B.ByteString -> Int -> Maybe (Int, B.ByteString)
starOneChar bs i =
  let hash = hashInput bs i
      pos = digit 6 hash
      p = B.head pos
      val = digit 7 hash
  in
    if (isDigit p && read [p] < 8)
    then Just (read [p], val)
    else Nothing

-- |
-- > >> solve2 "abc"
-- "05ace8e3"
--
-- I can't work out how to do this lazily, but also obey map semantics.
-- So I'm going to build a list of 20 then take 8 from that. :(
solve2 bs = B.concat . fmap snd . sortOn fst . take 8 . toList $ toMap
  where
    toMap :: Map Int B.ByteString
    toMap = Prelude.foldr (\(k, v) m -> insert k v m) (fromList []) stuff
    stuff :: [(Int, B.ByteString)]
    stuff = Prelude.take 20 . catMaybes $ fmap (starOneChar bs) (ints bs)


solveIO :: IO ()
solveIO = print $ B.unlines [solve1 input, solve2 input]
