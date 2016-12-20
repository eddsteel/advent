{-# LANGUAGE OverloadedStrings #-}
module DaySeven where

import Control.Applicative(many)
import Data.Char(isAlpha)
import Data.Text(Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Internal.Types as Atto

solve :: IO String
solve = do
  s <- readFile "inputs/seven.input"
  let ls = fmap T.pack . lines $ s
  return . unlines $ [solveStarOne ls] --, solveStarTwo ls]

solveStarOne :: [Text] -> String
solveStarOne ls = show . length . filter isTLS . fmap parseIPV7ABBA $ ls

solveStarTwo :: [Text] -> String
solveStarTwo ls = show . length . filter isSSL . fmap parseIPV7ABA $ ls

type Hyper = Text
type HyperS = String
type Super = Text
type SuperS = String
data IPV7ABBA = IPV71 Text [Super] [Hyper] deriving (Show, Eq)
-- abas in a list are overlapping
data IPV7ABA  = IPV72 Text [[SuperS]] [HyperS] deriving (Show, Eq)

-- |
-- >>> isTLS $ IPV71 "abcd[bddb]xyyx" ["bddb"] ["xyyx"]
-- False
-- >>> isTLS $ IPV71 "aaaa[qwer]tyui" [] []
-- False
-- >>> isTLS $ IPV71 "ioxxoj[asdfgh]zxcvbn" ["oxxo"] []
-- True
-- >>> isTLS $ IPV71 "abba[mnop]qrst" ["abba"] []
-- True
isTLS :: IPV7ABBA -> Bool
isTLS (IPV71 _ (_:_) []) = True
isTLS _ = False

-- |
-- >>> isSSL $ IPV72 "aba[bab]xyz" [["aba"]] ["bab"]
-- True
-- >>> isSSL $ IPV72 "xyx[xyx]xyx" [["xyx"]] ["xyx"]
-- False
-- >>> isSSL $ IPV72 "aaa[kek]eke" [["eke"]] ["kek"]
-- True
-- >>> isSSL $ IPV72 "zazbz[bzb]cdb" [["zaz","zbz"]] ["bzb"]
-- True
--
isSSL :: IPV7ABA -> Bool
isSSL (IPV72 _ [] _)  = False
isSSL (IPV72 _ _ [])  = False
isSSL (IPV72 _ sss hs) = all (correspondingLists hs) sss
  where
    correspondingLists :: [HyperS] -> [SuperS] -> Bool
    correspondingLists hs ss = any (\s -> corresponding hs s) ss
    corresponding :: [HyperS] -> SuperS -> Bool
    corresponding babs (a:b:c:[]) = a == c && elem (b:a:b:[]) babs


-- |
-- >>> parse abbaP "abba"
-- Done "" "abba"
--
-- >>> parse abbaP "oooo"
-- Fail "ooo" [] "Failed reading: satisfy"
--
abbaP :: Parser Super
abbaP = do
  a <- letter
  b <- satisfy $ \c -> isAlpha c && c /= a
  _ <- char b
  _ <- char a
  return $ T.pack [a, b, b, a]

-- |
-- >>> parse abaP "aba"
-- Done "" "aba"
abaP :: Parser SuperS
abaP = do
  a <- letter
  b <- satisfy $ \c -> isAlpha c && c /= a
  _ <- char a
  return [a, b, a]


-- |
-- Parse multiple "ABBA"s, discarding other letters.
--
-- >>> parseOnly abbasP "abba"
-- Right ["abba"]
-- >>> parseOnly abbasP "abbaoddo"
-- Right ["abba","oddo"]
-- >>> parseOnly abbasP "lkjcooclkja"
-- Right ["cooc"]
-- >>> parseOnly abbasP "lkjaldskjlajgds"
-- Right []
--
abbasP :: Parser [Super]
abbasP = fmap (foldr reduceE []) $ many1 $ eitherP abbaP (skip isAlpha)

reduceE :: (Either a b) -> [a] -> [a]
reduceE (Left a) as = a:as
reduceE (Right _) as = as


-- |
-- >> > parseOnly overlappingAbas "abab"
-- Right ["aba","bab"]
-- >> > parseOnly overlappingAbas "abaca"
-- Right ["aba","aca"]
--
overlappingAbas :: Parser [SuperS]
overlappingAbas = do
  a <- option "" (lookAhead abaP)
  b <- option "" (skip isAlpha *> lookAhead abaP)
  c <- option "" (skip isAlpha *> lookAhead abaP)
  return $ filter (/= "") [a,b,c]

-- |
-- >> > parseOnly abasP "abaodo"
-- Right [["aba"],["odo"]]
-- >> > parseOnly abasP "abab"
-- Right [["aba", "bab"]]
--
abasP :: Parser [[SuperS]]
abasP = fmap (foldr reduceE []) $ many1 (
  eitherP overlappingAbas (skip isAlpha))


-- |
--
-- >>> parseOnly hypernetAbbasP "[bddb]"
-- Right ["bddb"]
-- >>> parseOnly hypernetAbbasP "[qwer]"
-- Right []
-- >>> parseOnly hypernetAbbasP "[kjhasdabbalkja]"
-- Right ["abba"]
--
hypernetAbbasP :: Parser [Hyper]
hypernetAbbasP = "[" *> abbasP <* "]"

-- |
-- > >> parseOnly hyperBabsP "[lkjojolkj]"
-- Right [["joj"]]
--
hyperBabsP :: Parser [[HyperS]]
hyperBabsP = "[" *> abasP <* "]"

-- |
-- >>> parseOnly ipv71P "abba[oxxo]"
-- Right [Right ["abba"],Left ["oxxo"]]
--
-- >> > parseOnly ipv71P "aaaa[qwer]tyui"
-- Right [Right [],Left []]
--
-- > >> parseOnly ipv71P "ioxxoj[asffgh]zxcvbn[lkjwttwlkjl]"
-- Right [Right ["oxxo"],Left ["wttw"]]
--
ipv71P :: Parser [Either [Super] [Hyper]]
ipv71P = (:) <$> eitherP hypernetAbbasP abbasP <*> option [] ipv71P

ipv72P :: Parser [Either [[SuperS]] [[HyperS]]]
ipv72P = (:) <$> eitherP hyperBabsP abasP <*> option [] ipv72P

-- |
-- >> > parseIPV7ABBA "abcd[bddb]xyyx"
-- IPV71 "abcd[bddb]xyyx" ["xyyx"] ["bddb"]
-- >> > parseIPV7ABBA "nefefqadkmytguyp[ucqagcoyxinbrvbw]neksoxgtnnfojobtx[bxhdwvwfhybtbzkijj]poayieifsaocrboesfe[tnggfefcucifowqp]olmjwaqlaiwkkbtruw"
-- IPV71 "nefefqadkmytguyp[ucqagcoyxinbrvbw]neksoxgtnnfojobtx[bxhdwvwfhybtbzkijj]poayieifsaocrboesfe[tnggfefcucifowqp]olmjwaqlaiwkkbtruw" [] []
-- >> > parseIPV7ABBA "abba[mnop]qrst"
-- IPV71 "abba[mnop]qrst" ["abba"] []
--
-- >> >isTLS $ parseIPV7ABBA "abba[mnop]qrst"
-- True
-- >> >isTLS $ parseIPV7ABBA "abcd[bddb]xyyx"
-- False
-- >> >isTLS $ parseIPV7ABBA "aaaa[qwer]tyui"
-- False
-- >> >isTLS $ parseIPV7ABBA "ioxxoj[asdfgh]zxcvbn"
-- True
--
parseIPV7ABBA :: Text -> IPV7ABBA -- no bad input YOLOL XP
parseIPV7ABBA s = case parseOnly ipv71P s of
  (Right eithers) ->
    let (as, has) = consolidate eithers
    in IPV71 s has as
  _ -> error (T.unpack $ T.concat ["bad input ", s])

consolidate :: [Either [a] [b]] -> ([a], [b])
consolidate = foldr step ([],[])
  where step (Left a) (as, bs) = (a ++ as, bs)
        step (Right b) (as, bs) = (as, b ++ bs)

-- |
-- >> > parseIPV7ABA "aba[bab]xyz"
-- IPV72 "aba[bab]xyz" [["aba"]] ["bab"]
-- >> > parseIPV7ABA "xyx[xyx]xyx"
-- IPV72 "xyx[xyx]xyx" [["xyx"],["xyx"]] ["xyx"]
-- >> > parseIPV7ABA "aaa[kek]eke"
-- IPV72 "aaa[kek]eke" [["eke"]] ["kek"]
-- >> > parseIPV7ABA "zazbz[bzb]cdb"
-- IPV72 "zazbz[bzb]cdb" [["zaz","zbz"]] ["bzb"]
parseIPV7ABA :: Text -> IPV7ABA
parseIPV7ABA s = case parseOnly ipv72P s of
  (Right eithers) ->
    let (babs, abass) = consolidate eithers
    in IPV72 s abass (heads babs) -- no overlapping babs? It's unclear.
  _ -> error (T.unpack $ T.concat ["bad input ", s])


-- we only handle babs that don't overlap.
-- this is where that will cause visible error
heads :: [[a]] -> [a]
heads ([]) = []
heads ([[]]) = []
heads ([a]:as) = a:(heads as)
heads _ = error "unhandled input"


-- I straight up copied this off the attoparsec source
-- | Apply a parser without consuming any input.
lookAhead :: Atto.Parser i a -> Atto.Parser i a
lookAhead p = Atto.Parser $ \t pos more lose succ ->
  let succ' t' _pos' more' = succ t' pos more'
  in Atto.runParser p t pos more lose succ'
{-# INLINE lookAhead #-}
