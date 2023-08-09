-- TODO: tidy code
module Json where

import Control.Monad (guard, (>=>), void, ap, liftM, replicateM)
import Control.Applicative (Alternative((<|>), empty, many, some))
import Data.Char (chr, isDigit, isHexDigit)
import Data.Functor (($>))
import Data.List (foldl', uncons)

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
get = Parser uncons

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

-- Doesn't expect a trailing separator, empty result is valid
sepBy :: Parser separator -> Parser element -> Parser [element]
sepBy separator element =
  (:) <$> element <*> many (separator *> element) <|> pure []

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
  surround (char '"') (char '"') (many (escapedChar <|> regularChar))

digitsToInt :: Num a => String -> a
digitsToInt = fromIntegral . foldl' f 0
  where f n c = (n * 10) + charToInt c

digit :: Parser Char
digit = satisfy isDigit

jNumber :: Parser JValue
jNumber = JNumber <$> (f <$> sign <*> integer <*> fraction <*> exponent)
  where
    f sign integer fraction exponent = sign * (integer + fraction) ** exponent
    sign = char '-' $> -1 <|> pure 1
    integer = (digitsToInt <$> nonZeroInteger) <|> char '0' $> 0
    fraction = (digitsToFraction <$> (char '.' *> some digit)) <|> pure 0
    exponent = Json.exponent <|> pure 1

digitsToFraction :: [Char] -> Double
digitsToFraction digits = digitsToInt digits / fromIntegral (length digits)

nonZeroInteger :: Parser [Char]
nonZeroInteger = (:) <$> satisfy (`elem` ['1'..'9']) <*> many digit

exponent :: Parser Double
exponent = do
  char 'e' <|> char 'E'
  sign <- char '-' $> -1 <|> char '+' $> 1 <|> pure 1
  digits <- some digit
  pure $ sign * digitsToInt digits

jString :: Parser JValue
jString = JString <$> stringLiteral

ws :: Parser ()
ws = void $ many (char ' ' <|> char '\r' <|> char '\n' <|> char '\t')

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
