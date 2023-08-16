module Parse where

import Control.Applicative (Alternative (empty, (<|>), many), Applicative (liftA2))
import Control.Monad ((>=>), ap, liftM, guard)
import Data.Bifunctor (first, Bifunctor (bimap, second))
import Data.Char (isDigit, isHexDigit)
import Data.List (uncons)
import Data.Functor (($>))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

parse :: Parser a -> String -> Maybe a
parse (Parser f) text = case f text of
  Just (a, "") -> Just a
  _ -> Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser (\s -> pure (a, s))
  Parser pf <*> Parser pa = Parser (pf >=> \(f, s') -> first f <$> pa s')

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
  (Parser pa) >>= f = Parser (pa >=> (\(a, rest) -> runParser (f a) rest))

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
