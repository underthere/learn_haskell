{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- String -> Maybe (a, String) to String -> Maybe (b, String)
instance Functor Parser where
  f `fmap` (Parser {runParser=run}) = Parser {runParser = fmap (first f) . run}

-- Exercise 2
instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  -- f :: String -> Maybe ((a->b), String)
  -- pr :: String -> Maybe (a, String)
  -- result :: String -> Maybe (b, String)
  p0@(Parser f) <*> p1@(Parser pr) = Parser result
    where
      result str = case f str of
        Nothing -> Nothing
        Just (f', str') -> first f' <$> pr str'

-- Exercise 3

-- (,) :: a -> b -> (a, b)
-- char Char :: Parser (String -> Maybe (Char, String))
-- (,) <$> char Char :: Parser (String -> Maybe ((Char,), String))
-- (,) <$> char Char <*> char Char :: Parser
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = () <$ abParser

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser f
    where
      f str = runParser p1 str <|> runParser p2 str

-- Exercise 5
intOrUppercase :: Parser ()
intOrUppercase = () <$ posInt <|> () <$ satisfy (\x -> isUpper x)
