import Control.Monad
import Control.Applicative
import Data.Char

data LKC = VAR String | NUM Int | NULL | ADD LKC LKC |
           SUB LKC LKC | MULT LKC LKC | DIV LKC LKC |
           EQ LKC LKC | LEQ LKC LKC | H LKC | T LKC | CONS LKC LKC |
           IF LKC LKC LKC | LAMBDA [LKC] LKC | CALL LKC [LKC] |
           LET LKC [(LKC,LKC)] | LETREC LKC [(LKC, LKC)]
           deriving(Show, Eq)

newtype Parser a = P(String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P parser) input = parser input

instance Functor Parser where
    -- (a -> b) -> Parser a -> Parser b
    fmap f parser = P(\input -> case parse parser input of 
                                    [] -> []
                                    [(v,other)] -> [(f v,other)])

instance Applicative Parser where
    -- a -> Parser a
    pure v = P(\input -> [(v,input)])

    -- Parser (a -> b) -> Parser a -> Parser b
    parserf <*> parserx = P(\input -> case parse parserf input of 
                                        [] -> []
                                        [(f,other)] -> parse (fmap f parserx) other)

instance Monad Parser where
    -- a -> Parser a
    return = pure

    -- Parser a -> (a -> Parser b) -> Parser b
    parser >>= f = P(\input -> case parse parser input of 
                                [] -> []
                                [(v,other)] -> parse (f v) other)

instance Alternative Parser where
    -- Parser a
    empty = P(\input->[])

    -- Parser a -> Parser a -> Parser a
    parserx <|> parsery = P(\input -> case parse parserx input of 
                                        [] -> parse parsery input
                                        [(v,other)] -> [(v, other)])
    
    -- must work at least once otherwise it fails
    -- Parser a -> Parser [a]
    some x = pure (:) <*> x <*> many x

    -- could work 0 or more times, it never fails
    -- Parser a -> Parser [a]
    many x = some x <|> pure []

item :: Parser Char
item = P(\input -> case input of 
                        [] -> []
                        (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat predicate = do x <- item
                   if (predicate x) 
                       then return x
                       else empty   

--check if the first character of a string is a digit
digit :: Parser Char
digit = sat isDigit

--check if the first character of a string is a lower case
lower :: Parser Char
lower = sat isLower

--check if the first character of a string is a upper case
upper :: Parser Char
upper = sat isUpper

--check if the first character of a string is a letter
letter :: Parser Char
letter = sat isAlpha

--check if the first character of a string is an alphanumeric character
alphanum :: Parser Char
alphanum = sat isAlphaNum

--check if a string start with a certain character
char :: Char -> Parser Char
char c = sat (== c)

--check if a string start with a certain substring
string :: String -> Parser String
string [] = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)

--check if a string start with a lower case letter (if it is an identifier)
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

--parse a natural number
nat :: Parser String
nat = do xs <- some digit
         return (read xs)

--remove one or more spaces
space :: Parser ()
space = do many (sat isSpace)
           return ()

--parse an int number: first it tries to read a '-' sign(negative integer), if it fails it tries to read a natural number(positive integer)
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|>
         nat

--given a parser, remove the first spaces
token :: Parser a -> Parser a
token parser = do space
                  value <- parser
                  space
                  return value

--parse a identifier
identifier :: Parser String
identifier = token ident

--parse a natural number
natural :: Parser Int
natural = token nat

--parse an integer
integer :: Parser Int
integer = token int

--parse a symbol
symbol :: String -> Parser String
symbol xs = token (string xs)

--parse an array of integer
nats :: Parser [Int]
nats = do symbol '['
          n <- natural
          ns <- many (do symbol ','
                         natural)
          symbol ']'
          return (n:ns)
