-- TODO: tidy code
module Json where

import Control.Monad (guard, (>=>), void, ap, liftM, replicateM)
import Data.Bifunctor (Bifunctor(first))
import Control.Applicative (Alternative((<|>), empty), liftA3)
import Data.Char (isSpace, chr, ord, isDigit, isHexDigit)
import Data.Functor (($>))
import Data.List (foldl')

-- All the piping to get it to work (an ad-hoc parsing library)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (<*>) = ap

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
  (Parser pa) >>= f = Parser (pa >=> (\(a, rest) -> parse (f a) rest))

parseAll :: Parser a -> String -> Maybe a
parseAll (Parser parser) input = do
  (value, rest) <- parser input
  guard $ null rest
  pure value

get :: Parser Char
get = Parser f
  where
    f [] = Nothing
    f (char:rest) = Just (char, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = do
  c <- get
  guard $ predicate c
  pure c

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

regularChar :: Parser Char
regularChar = satisfy (\c -> c /= '"' && c /= '\\')

escapedChar :: Parser Char
escapedChar = char '\\' >> (unicodeChar <|> (get >>= matchEscapedChar))

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

hexDigitsToInt :: [Char] -> Int
hexDigitsToInt = foldl' f 0
  where f acc n = acc * 16 + charToInt n

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt 'a' = 10
charToInt 'b' = 11
charToInt 'c' = 12
charToInt 'd' = 13
charToInt 'e' = 14
charToInt 'f' = 15
charToInt 'A' = 10
charToInt 'B' = 11
charToInt 'C' = 12
charToInt 'D' = 13
charToInt 'E' = 14
charToInt 'F' = 15

unicodeChar :: Parser Char
unicodeChar = chr . hexDigitsToInt <$> (char 'u' >> replicateM 4 hexDigit)

matchEscapedChar :: Alternative m => Char -> m Char
matchEscapedChar '"' = pure '\"'
matchEscapedChar '/' = pure '/'
matchEscapedChar '\\' = pure '\\'
matchEscapedChar 'b' = pure '\b'
matchEscapedChar 'f' = pure '\f'
matchEscapedChar 'n' = pure '\n'
matchEscapedChar 'r' = pure '\r'
matchEscapedChar 't' = pure '\t'
matchEscapedChar _ = empty

stringLiteral :: Parser String
stringLiteral =
  surround (char '"') (char '"') (zeroOrMore (escapedChar <|> regularChar))

digitsToInt :: String -> Int
digitsToInt = foldl' f 0
  where f n c = (n * 10) + charToInt c

digit :: Parser Char
digit = satisfy isDigit

int :: Parser Int
int = digitsToInt <$> oneOrMore digit

double :: Parser Double
double = parseDouble <$> oneOrMore digit <*> string "." <*> oneOrMore digit
  where parseDouble a b c = read (a ++ b ++ c)

-- TODO: other formats e.g. scientific notation
-- TODO: negative numbers
jNumber :: Parser JValue
jNumber = JNumber <$> (double <|> fromIntegral <$> int)

jString :: Parser JValue
jString = JString <$> stringLiteral

ws :: Parser ()
ws = void $ zeroOrMore (char ' ' <|> char '\r' <|> char '\n' <|> char '\t')

emptyList :: Parser [a]
emptyList = ws $> []

jArray :: Parser JValue
jArray = JArray <$> surround (char '[') (char ']') (values <|> emptyList)
  where values = sepBy (char ',') jValue

jObject :: Parser JValue
jObject =
  JObject <$> surround (char '{') (char '}') (sepBy (char ',') kv <|> emptyList)
  where kv = (,) <$> (ws *> stringLiteral) <*> (ws *> char ':' *> jValue)

jValue :: Parser JValue
jValue = surround ws ws actual
  where actual = jNull <|> jBool <|> jNumber <|> jString <|> jArray <|> jObject
