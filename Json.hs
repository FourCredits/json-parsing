module Json where

import Control.Monad (guard, (>=>), void)
import Data.Bifunctor (Bifunctor(first))
import Control.Applicative (Alternative((<|>), empty), liftA3)
import Data.Char (isSpace, ord, isDigit)
import Data.Functor (($>))
import Data.List (foldl')

-- All the piping to get it to work (an ad-hoc parsing library)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f pa = Parser (fmap (first f) . parse pa)

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  pf <*> pa = Parser (\s -> do
    (f, r1) <- parse pf s
    (a, r2) <- parse pa r1
    return (f a, r2))

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser (\s -> parse p1 s <|> parse p2 s)

instance Monad Parser where
  return = pure
  pa >>= f = Parser (parse pa >=> (\(a, rest) -> parse (f a) rest))

parseAll :: Parser a -> String -> Maybe a
parseAll parser input =
  case parse parser input of
    Just (value, "") -> Just value
    _ -> Nothing

get :: Parser Char
get = Parser f
  where
    f [] = Nothing
    f (char:rest) = Just (char, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = get >>= (\c -> guard (predicate c) >> return c)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

surround :: Parser open -> Parser close -> Parser p1 -> Parser p1
surround open close inner = open *> inner <* close

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

whitespace :: Parser ()
whitespace = void $ zeroOrMore $ satisfy isSpace

-- Doesn't expect a trailing separator, empty result is valid
sepBy :: Parser separator -> Parser element -> Parser [element]
sepBy separator element =
  (:) <$> element <*> zeroOrMore (separator *> element) <|> pure []

-- The actual parsing

data JValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Show, Eq)

jNull :: Parser JValue
jNull = JNull <$ string "null"

jBool :: Parser JValue
jBool = JBool True <$ string "true" <|> JBool False <$ string "false"

-- TODO: escaping
stringLiteral :: Parser String
stringLiteral = surround (char '"') (char '"') (zeroOrMore (satisfy (/= '"')))

charsToInt :: String -> Int
charsToInt = foldl' f 0
  where f n c = (n * 10) + (ord c - ord '0')

digit :: Parser Char
digit = satisfy isDigit

int :: Parser Int
int = charsToInt <$> oneOrMore digit

double :: Parser Double
double = parseDouble <$> oneOrMore digit <*> string "." <*> oneOrMore digit
  where parseDouble a b c = read (a ++ b ++ c)

-- TODO: other formats e.g. scientific notation
jNumber :: Parser JValue
jNumber = JNumber <$> (double <|> fromIntegral <$> int)

jString :: Parser JValue
jString = JString <$> stringLiteral

jArray :: Parser JValue
jArray = JArray <$> surround start end (sepBy sep jValue)
  where
    start = whitespace *> char '[' <* whitespace
    end = whitespace *> char ']' <* whitespace
    sep = whitespace *> char ',' <* whitespace

jObject :: Parser JValue
jObject = JObject <$> surround start end (sepBy sep kv)
  where
    start = whitespace *> char '{' <* whitespace
    end = whitespace *> char '}' <* whitespace
    sep = whitespace *> char ',' <* whitespace
    kv = liftA3 (\k _ v -> (k, v)) stringLiteral kvSep jValue
    kvSep = whitespace *> char ':' <* whitespace

jValue :: Parser JValue
jValue = jNull <|> jBool <|> jNumber <|> jString <|> jArray <|> jObject
