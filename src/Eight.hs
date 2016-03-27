module Eight where


-- | Length of the uninterpreted string.
--
-- >>> lengthString "\"\""
-- 2
-- >>> lengthString "\"abc\""
-- 5
-- >>> lengthString "\"aaa\\\"aaa\""
-- 10
-- >>> lengthString "\"\\x27\""
-- 6
--
lengthString :: String -> Int
lengthString = length

-- | Length of the value as read from the string.
--
-- >>> lengthValue "\"\""
-- 0
-- >>> lengthValue "\"abc\""
-- 3
-- >>> lengthValue "\"aaa\\\"aaa\""
-- 7
-- >>> lengthValue "\"\\x27\""
-- 1
--
lengthValue :: String -> Int
lengthValue s = length ((read s) :: String)


-- |
-- >>> weirdSum ["\"\"", "\"abc\"", "\"aaa\\\"aaa\"", "\"\\x27\""]
-- 12
--
weirdSum :: [String] -> Int
weirdSum strings = (sum (fmap lengthString strings)) - (sum (fmap lengthValue strings))


-- |
-- >>> starOne "\"\"\n\"abc\"\n\"aaa\\\"aaa\"\n\"\\x27\""
-- "12"
--
starOne :: String -> String
starOne = show . weirdSum . lines


main :: IO ()
main = interact starOne
