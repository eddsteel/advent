{-# LANGUAGE OverloadedStrings #-}
module DayFour
where

import Data.Char
import Data.List(group, sortBy, sort)
import Data.Text(Text)
import qualified Data.Text as T
import Text.ParserCombinators.Parsec


solve :: IO Text
solve = do
  ls <- lines <$> readFile "inputs/four.input"
  let rss = fmap parseRS ls
  let total = sumSectors rss
  let s1 = T.pack $ show total
  let s2 = filter (T.isPrefixOf "northpole object storage") $ fmap decrypt rss
  return $ T.concat (s1 : "\n" : s2)

data RoomSpec = RS {name :: Text, sector :: Int, checksum :: String} deriving (Show,Read)

-- |
-- >>> calcCS "aaaaa-bbb-z-y-x"
-- "abxyz"
-- >>> calcCS "not-a-real-room"
-- "oarel"
calcCS :: Text -> String
calcCS = take 5 . reverse . fmap head . sortBy comp . group . sort . filter (/= '-') . T.unpack


-- |
-- >>> comp "zzzz" "bb"
-- GT
-- >>> comp "z" "d"
-- LT
-- >>> comp "aaa" "aaa"
-- EQ
comp :: String -> String -> Ordering
comp as bs =
  case length as `compare` length bs of
    EQ -> head bs `compare` head as
    a -> a

-- |
-- >>> verify RS {name = "aaaaa-bbb-z-y-x", sector = 123, checksum = "abxyz"}
-- True
-- >>> verify RS {name = "a-b-c-d-e-f-g-h", sector = 987, checksum = "abcde"}
-- True
-- >>> verify RS {name = "not-a-real-room", sector = 404, checksum = "oarel"}
-- True
-- >>> verify RS {name = "totally-real-room", sector = 200, checksum = "decoy"}
-- False
--
verify :: RoomSpec -> Bool
verify rs = (checksum rs) == (calcCS $ name rs)

-- |
-- >>> :{
--  sumSectors [ RS {name = "aaaaa-bbb-z-y-x", sector = 123, checksum = "abxyz"}
--             , RS {name = "a-b-c-d-e-f-g-h", sector = 987, checksum = "abcde"}
--             , RS {name = "not-a-real-room", sector = 404, checksum = "oarel"}
--             , RS {name = "totally-real-room", sector = 200, checksum = "decoy"}]
-- :}
-- 1514
--
sumSectors :: [RoomSpec] -> Int
sumSectors rss  = sum $ sector <$> filter verify rss

-- |
-- >>> parse roomspecParser "" "aaaaa-bbb-z-y-x-123[abxyz]"
-- Right (RS {name = "aaaaa-bbb-z-y-x", sector = 123, checksum = "abxyz"})
-- >>> parse roomspecParser "" "a-b-c-d-e-f-g-h-987[abcde]"
-- Right (RS {name = "a-b-c-d-e-f-g-h", sector = 987, checksum = "abcde"})
-- >>> parse roomspecParser "" "not-a-real-room-404[oarel]"
-- Right (RS {name = "not-a-real-room", sector = 404, checksum = "oarel"})
-- >>> parse roomspecParser "" "totally-real-room-200[decoy]"
-- Right (RS {name = "totally-real-room", sector = 200, checksum = "decoy"})
roomspecParser :: GenParser Char st RoomSpec
roomspecParser = do
  n <- many1 ((char '-') <|> letter)
  s <- many1 digit
  char '['
  cs <- many (noneOf "]")
  return RS {name = T.pack (init n), sector = read s, checksum = cs}


parseRS :: String -> RoomSpec
parseRS s = case parse roomspecParser "" s of
  Right rs -> rs
  Left _ -> error "nuh uh"

-- |
-- >>> cipher 343 'q'
-- 'v'
-- >>> fmap (cipher 343) "qzmt" :: [Char]
-- "very"
-- >>> fmap (cipher 343) "qzmt-zixmtkozy-ivhz" :: [Char]
-- "very encrypted name"
--
cipher :: Int -> Char -> Char
cipher _ '-' = ' '
cipher i c =
  let
    l = ord c - 97
    l' = (l + i) `mod` 26
  in
   chr (l' + 97)

-- |
-- >>> decrypt (RS {name="qzmt-zixmtkozy-ivhz", sector=343, checksum=""})
-- "very encrypted name (343)\n"
decrypt :: RoomSpec -> Text
decrypt rs = T.concat [ T.pack $ fmap (cipher (sector rs)) (T.unpack (name rs))
                      , " (" , T.pack $ show $ sector rs, ")\n"]
