-- A deliberately small EDN subset, not a query DSL.
module Datomic.EDN (EDN(..), parseEDN, renderEDN, field) where

import Data.Char (chr, isControl, ord)
import Numeric (readHex, showHex)
import Text.Parsec hiding (token)
import Text.Parsec.String (Parser)

data EDN = Nil | Boolean Bool | IntEDN Integer | StringEDN String
         | Keyword String | Symbol String | Vector [EDN] | List [EDN]
         | Map [(EDN, EDN)] | Set [EDN] | Tagged String EDN
         deriving (Eq, Show)

field :: String -> EDN -> Maybe EDN
field k (Map xs) = lookup (Keyword k) xs
field _ _ = Nothing

renderEDN :: EDN -> String
renderEDN Nil = "nil"
renderEDN (Boolean b) = if b then "true" else "false"
renderEDN (IntEDN n) = show n
renderEDN (StringEDN s) = '"' : concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | isControl c = let h = showHex (ord c) "" in "\\u" ++ replicate (4 - length h) '0' ++ h
             | otherwise = [c]
renderEDN (Keyword k) = ":" ++ token k
renderEDN (Symbol s) = token s
renderEDN (Vector xs) = collection "[" "]" xs
renderEDN (List xs) = collection "(" ")" xs
renderEDN (Set xs) = collection "#{" "}" xs
renderEDN (Map xs) = collection "{" "}" (concatMap (\(k,v) -> [k,v]) xs)
renderEDN (Tagged t v) = "#" ++ token t ++ " " ++ renderEDN v

token :: String -> String
token s | not (null s) && all (`notElem` " \t\r\n,[]{}()\";\\#:") s = s
        | otherwise = error "Invalid EDN token"

collection :: String -> String -> [EDN] -> String
collection a b xs = a ++ unwords (map renderEDN xs) ++ b

parseEDN :: String -> Either String EDN
parseEDN s = either (Left . show) Right (parse (spaceEDN *> value <* eof) "EDN" s)

spaceEDN :: Parser ()
spaceEDN = skipMany (oneOf " \t\r\n," <|> (char ';' *> many (noneOf "\n") *> pure '\n'))

value :: Parser EDN
value = (choice
  [ StringEDN <$> quoted
  , Vector <$> enclosed '[' ']' (many value)
  , List <$> enclosed '(' ')' (many value)
  , Map <$> enclosed '{' '}' (many ((,) <$> value <*> value))
  , char '#' *> ((Set <$> enclosed '{' '}' (many value)) <|> (Tagged <$> atom <* spaceEDN <*> value))
  , char ':' *> (Keyword <$> atom)
  , atom >>= scalar
  ]) <* spaceEDN
  where
    scalar "nil" = pure Nil
    scalar "true" = pure (Boolean True)
    scalar "false" = pure (Boolean False)
    scalar a = case reads a of
      [(n, "")] -> pure (IntEDN n)
      _ | head a `elem` "0123456789" -> fail "Unsupported EDN numeric value"
        | otherwise -> pure (Symbol a)

enclosed :: Char -> Char -> Parser a -> Parser a
enclosed a b p = char a *> spaceEDN *> p <* char b

atom :: Parser String
atom = many1 (noneOf " \t\r\n,[]{}()\";\\#:")

quoted :: Parser String
quoted = char '"' *> many (noneOf "\"\\" <|> (char '\\' *> escaped)) <* char '"'
  where
    escaped = choice [char '"' *> pure '"', char '\\' *> pure '\\', char 'n' *> pure '\n',
                      char 'r' *> pure '\r', char 't' *> pure '\t', char 'b' *> pure '\b',
                      char 'f' *> pure '\f', char 'u' *> unicode]
    unicode = do
      h <- count 4 hexDigit
      pure (chr (fst (head (readHex h))))
