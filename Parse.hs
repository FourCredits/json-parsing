module Parse where

import Control.Applicative (Alternative (empty, (<|>), many))
import Control.Monad ((>=>), ap, liftM, guard)
import Data.Char (isDigit, isHexDigit)
import Data.List (uncons)
import Data.Functor (($>))

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

get :: Parser Char
get = Parser uncons

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = get >>= (\c -> guard (predicate c) $> c)

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

string :: String -> Parser String
string = traverse char

surround :: Parser open -> Parser close -> Parser p1 -> Parser p1
surround open close inner = open *> inner <* close

-- | Doesn't expect a trailing separator, empty result is valid
sepBy :: Parser separator -> Parser element -> Parser [element]
sepBy separator element =
  (:) <$> element <*> many (separator *> element) |> []

(|>) :: Alternative m => m a -> a -> m a
ma |> a = ma <|> pure a
infixl 2 |>
