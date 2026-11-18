-- EDN values used by Datomic, not a query DSL. DecimalEDN c e means c * 10^e.
module Datomic.EDN (EDN(..), parseEDN, encodeEDN, renderEDN, field) where

import Data.Char (chr, isControl, isDigit, ord)
import Data.Int (Int64)
import Data.List (intercalate)
import Numeric (readHex, showHex)
import Text.Parsec hiding (token)
import Text.Parsec.String (Parser)

data EDN = Nil | Boolean Bool | IntEDN Integer | BigIntEDN Integer
         | FloatEDN Double | DecimalEDN Integer Int | StringEDN String
         | Keyword String | Symbol String | Vector [EDN] | List [EDN]
         | Map [(EDN, EDN)] | Set [EDN] | Tagged String EDN
         deriving (Eq, Show)

field :: String -> EDN -> Maybe EDN
field k (Map xs) = lookup (Keyword k) xs
field _ _ = Nothing

-- Prefer encodeEDN for values constructed from untrusted input. renderEDN is
-- retained as a convenience for known-valid literals and diagnostics.
renderEDN :: EDN -> String
renderEDN = either error id . encodeEDN

encodeEDN :: EDN -> Either String String
encodeEDN Nil = Right "nil"
encodeEDN (Boolean b) = Right (if b then "true" else "false")
encodeEDN (IntEDN n)
  | n >= toInteger (minBound :: Int64) && n <= toInteger (maxBound :: Int64) = Right (show n)
  | otherwise = Left "IntEDN is outside signed 64-bit range; use BigIntEDN"
encodeEDN (BigIntEDN n) = Right (show n ++ "N")
encodeEDN (FloatEDN n)
  | isNaN n = Right "##NaN"
  | isInfinite n = Right (if n < 0 then "##-Inf" else "##Inf")
  | otherwise = Right (show n)
encodeEDN (DecimalEDN c e) = Right (show c ++ "e" ++ show e ++ "M")
encodeEDN (StringEDN s)
  | any surrogate s = Left "String contains an unpaired Unicode surrogate"
  | otherwise = Right ('"' : concatMap escape s ++ "\"")
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | isControl c = let h = showHex (ord c) "" in "\\u" ++ replicate (4 - length h) '0' ++ h
             | otherwise = [c]
encodeEDN (Keyword k) = (":" ++) <$> checkedToken k
encodeEDN (Symbol s)
  | s `elem` ["nil", "true", "false"] || numericStart s = Left "Invalid EDN symbol"
  | otherwise = checkedToken s
encodeEDN (Vector xs) = collection "[" "]" xs
encodeEDN (List xs) = collection "(" ")" xs
encodeEDN (Set xs) = collection "#{" "}" xs
encodeEDN (Map xs) = collection "{" "}" (concatMap (\(k,v) -> [k,v]) xs)
encodeEDN (Tagged t v) = do
  tag <- checkedToken t
  if numericStart t then Left "Invalid EDN tag" else do
    encoded <- encodeEDN v
    pure ("#" ++ tag ++ " " ++ encoded)

checkedToken :: String -> Either String String
checkedToken s
  | not (null s) && head s /= ':' && last s /= ':'
    && all (\c -> not (isControl c) && c `notElem` " \t\r\n,[]{}()\";\\#") s = Right s
  | otherwise = Left "Invalid EDN token"

numericStart :: String -> Bool
numericStart (c:_) | isDigit c = True
numericStart (sign:c:_) | sign `elem` "+-" && isDigit c = True
numericStart _ = False

surrogate :: Char -> Bool
surrogate c = ord c >= 0xd800 && ord c <= 0xdfff

collection :: String -> String -> [EDN] -> Either String String
collection a b xs = (\parts -> a ++ intercalate " " parts ++ b) <$> mapM encodeEDN xs

parseEDN :: String -> Either String EDN
parseEDN s = either (Left . show) Right (parse (spaceEDN *> value <* eof) "EDN" s)

spaceEDN :: Parser ()
spaceEDN = skipMany (oneOf " \t\r\n," <|> (char ';' *> many (noneOf "\n") *> pure '\n'))

value :: Parser EDN
value = (choice
  [ try (string "##-Inf") *> pure (FloatEDN (-1/0))
  , try (string "##Inf") *> pure (FloatEDN (1/0))
  , try (string "##NaN") *> pure (FloatEDN (0/0))
  , StringEDN <$> quoted
  , Vector <$> enclosed '[' ']' (many value)
  , List <$> enclosed '(' ')' (many value)
  , Map <$> enclosed '{' '}' (many ((,) <$> value <*> value))
  , char '#' *> ((Set <$> enclosed '{' '}' (many value)) <|> (Tagged <$> atom <* spaceEDN <*> value))
  , char ':' *> (atom >>= either fail (pure . Keyword) . checkedToken)
  , atom >>= scalar
  ]) <* spaceEDN
  where
    scalar "nil" = pure Nil
    scalar "true" = pure (Boolean True)
    scalar "false" = pure (Boolean False)
    scalar a
      | numericStart a = either fail pure (parseNumber a)
      | otherwise = either fail (const (pure (Symbol a))) (checkedToken a)

parseNumber :: String -> Either String EDN
parseNumber = either (Left . show) Right . parse (number <* eof) "EDN number"
  where
    number :: Parser EDN
    number = do
      sign <- option "" ((:[]) <$> oneOf "+-")
      whole <- many1 asciiDigit
      fraction <- optionMaybe (char '.' *> many asciiDigit)
      powerPart <- optionMaybe (oneOf "eE" *> ((++) <$> option "" ((:[]) <$> oneOf "+-") <*> many1 asciiDigit))
      suffix <- optionMaybe (oneOf "NM")
      let negative = if sign == "-" then negate else id
          integer = negative (read whole :: Integer)
          signed = (if sign == "+" then "" else sign) ++ whole
          floating = signed ++ maybe "" (\f -> '.' : if null f then "0" else f) fraction ++ maybe "" ('e':) powerPart
      case suffix of
        Just 'N' | fraction == Nothing && powerPart == Nothing -> pure (BigIntEDN integer)
        Just 'M' -> do
          let digits = maybe "" id fraction
              coefficient = negative (read (whole ++ digits) :: Integer)
              power = maybe 0 (read . dropPlus) powerPart - toInteger (length digits)
          if power < toInteger (minBound :: Int) || power > toInteger (maxBound :: Int)
            then fail "Decimal exponent out of range"
            else pure (DecimalEDN coefficient (fromInteger power))
        Nothing | fraction == Nothing && powerPart == Nothing ->
          if integer < toInteger (minBound :: Int64) || integer > toInteger (maxBound :: Int64)
            then fail "Integer outside signed 64-bit range; use N suffix"
            else pure (IntEDN integer)
        Nothing -> case reads floating of
          [(n, "")] | not (isInfinite n) -> pure (FloatEDN n)
          _ -> fail "Invalid or overflowing float"
        _ -> fail "Invalid numeric suffix"
    asciiDigit = oneOf "0123456789"
    dropPlus ('+':xs) = xs
    dropPlus xs = xs

enclosed :: Char -> Char -> Parser a -> Parser a
enclosed a b p = char a *> spaceEDN *> p <* char b

atom :: Parser String
atom = many1 (noneOf " \t\r\n,[]{}()\";\\#")

quoted :: Parser String
quoted = char '"' *> many character <* char '"'
  where
    character = (char '\\' *> escaped) <|> satisfy (\c -> c /= '"' && c /= '\\' && not (isControl c) && not (surrogate c))
    escaped = choice [char '"' *> pure '"', char '\\' *> pure '\\', char 'n' *> pure '\n',
                      char 'r' *> pure '\r', char 't' *> pure '\t', char 'b' *> pure '\b',
                      char 'f' *> pure '\f', char 'u' *> unicode]
    hex :: Parser Int
    hex = do
      h <- count 4 hexDigit
      pure (fst (head (readHex h)))
    unicode = do
      high <- hex
      if high >= 0xd800 && high <= 0xdbff then do
        _ <- string "\\u"
        low <- hex
        if low >= 0xdc00 && low <= 0xdfff
          then pure (chr (0x10000 + (high - 0xd800) * 0x400 + low - 0xdc00))
          else fail "Expected a low Unicode surrogate"
      else if high >= 0xdc00 && high <= 0xdfff then fail "Unpaired Unicode surrogate"
      else pure (chr high)
