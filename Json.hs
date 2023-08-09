module Json (JValue(..), jValue) where

import Control.Applicative (Alternative((<|>), empty, many, some))
import Control.Monad (void, replicateM, liftM4)
import Data.Char (chr)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (fromJust)
import Parse

data JValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Show, Eq)

jValue :: Parser JValue
jValue = surround ws ws actual
  where actual = jNull <|> jBool <|> jNumber <|> jString <|> jArray <|> jObject

ws :: Parser ()
ws = void $ many (char ' ' <|> char '\r' <|> char '\n' <|> char '\t')

jNull :: Parser JValue
jNull = JNull <$ string "null"

jBool :: Parser JValue
jBool = JBool True <$ string "true" <|> JBool False <$ string "false"

jNumber :: Parser JValue
jNumber = JNumber <$> liftM4 f sign integer fraction exponent
  where
    f sign integer fraction exponent = sign * (integer + fraction) ** exponent
    sign = char '-' $> -1 |> 1
    integer = (digitsToInt 10 <$> nonZeroInteger) <|> char '0' $> 0
    fraction = (digitsToFraction <$> (char '.' *> some digit)) |> 0
    exponent = Json.exponent |> 1
    nonZeroInteger = (:) <$> satisfy (`elem` ['1'..'9']) <*> many digit

digitsToFraction :: [Char] -> Double
digitsToFraction digits = digitsToInt 10 digits / fromIntegral (length digits)

digitsToInt :: Num a => Int -> String -> a
digitsToInt base = fromIntegral . foldl' f 0
  where
    f n c = (n * base) + charToInt c
    charToInt c = fromJust $ lookup c mapping
    mapping =
      [('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6),
       ('7', 7), ('8', 8), ('9', 9), ('A', 10), ('a', 10), ('B', 11), ('b', 11),
       ('C', 12), ('c', 12), ('D', 13), ('d', 13), ('E', 14), ('e', 14),
       ('F', 15), ('f', 15)]

exponent :: Parser Double
exponent = do
  char 'e' <|> char 'E'
  sign <- char '-' $> -1 <|> char '+' $> 1 |> 1
  digits <- some digit
  pure $ sign * digitsToInt 10 digits

jString :: Parser JValue
jString = JString <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = surround (char '"') (char '"') (many (escapedChar <|> normal))
  where normal = satisfy (\c -> c /= '"' && c /= '\\')

escapedChar :: Parser Char
escapedChar = char '\\' *> (unicodeChar <|> (get >>= matchEscapedChar))
  where
    matchEscapedChar c = maybe empty pure $ lookup c escapes
    escapes =
      [('"', '"'), ('/', '/'), ('\\', '\\'), ('b', '\b'), ('f', '\f'),
       ('n', '\n'), ('r', '\r'), ('t', '\t')]

unicodeChar :: Parser Char
unicodeChar = chr . digitsToInt 16 <$> (char 'u' *> replicateM 4 hexDigit)

jArray :: Parser JValue
jArray = JArray <$> surround (char '[') (char ']') (values <|> ws $> [])
  where values = sepBy (char ',') jValue

jObject :: Parser JValue
jObject =
  JObject <$> surround (char '{') (char '}') (sepBy (char ',') kv <|> ws $> [])
  where kv = (,) <$> (ws *> stringLiteral) <*> (ws *> char ':' *> jValue)
