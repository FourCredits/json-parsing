module Parse where

import Control.Applicative (Alternative (empty, (<|>), many), Applicative (liftA2))
import Control.Monad ((>=>), guard)
import Data.Bifunctor (first)
import Data.Char (isDigit, isHexDigit)
import Data.List (uncons)
import Data.Functor (($>))

data State = State String Int Int deriving (Show, Eq)
newtype Parser a =
  Parser { runParser :: State -> Either (Int, Int) (a, State) }

parse :: Parser a -> String -> Either (Int, Int) a
parse (Parser f) text = do
  (value, State remaining line column) <- f (State text 1 0)
  if null remaining then Right value else Left (line, column)

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser (\s -> pure (a, s))
  Parser pf <*> Parser pa = Parser (pf >=> \(f, s') -> first f <$> pa s')

instance Alternative Parser where
  empty = Parser $ \(State _ l c) -> Left (1, 0)
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <> p2 s

instance Monad Parser where
  (Parser pa) >>= f = Parser (pa >=> (\(a, rest) -> runParser (f a) rest))

get :: Parser Char
get = Parser f
  where
    f (State ('\n':cs) line column) = Right ('\n', State cs (line + 1) 0)
    f (State (c:cs) line column) = Right (c, State cs line (column + 1))
    f (State [] line column) = Left (line, column)

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
