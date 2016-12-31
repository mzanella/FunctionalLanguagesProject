import Control.Monad
import Control.Applicative
import Data.Char

-- let x=2 and y=4 in x+y*2 end
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
nat :: Parser Int
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
         return (-1 * n)
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
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

--parse a sequence of variables
parserSeqVar :: Parser String
parserSeqVar = do x <- identifier
                  var <- parserSeqVar
                  return (x ++ " " ++ var)
                  <|>
                  return ""

--parse an operation with prefix operator
parserOPP :: Parser String
parserOPP = do symbol "cons"
               return "cons"
            <|>
            do symbol "head"
               return "head"
            <|>
            do symbol "tail"
               return "tail"
            <|>
            do symbol "eq"
               return "eq"
            <|>
            do symbol "leq"
               return "leq"

--parse a multiplicative operation with infix operator
parserOPM :: Parser String
parserOPM = do symbol "*"
               return "*"
            <|>
            do symbol "/"
               return "/"

--parse a aritmetic operation with infix operator
parserOPA :: Parser String
parserOPA = do symbol "+"
               return "+"
            <|>
            do symbol "-"
               return "-"

parserExpa :: Parser String
parserExpa = do term <- parserTerm
                do opa <- parserOPA
                   expa <- parserExpa
                   return (term ++ opa ++ expa)
                   <|>
                   return term

parserFactor :: Parser String
parserFactor = do x <- identifier
                  (do y <- parserY
                      return (x ++ y)
                   <|>
                   return x)
               <|>
               do num <- int
                  return (show num)
               <|>
               do symbol "null"
                  return "null"
               <|>
               do symbol "("
                  expa <- parserExpa
                  symbol ")"
                  return ("(" ++ expa ++ ")")

parserTerm :: Parser String
parserTerm = do factor <- parserFactor
                do opm <- parserOPM
                   term <- parserTerm
                   return (factor ++ opm ++ term)
                   <|>
                   return factor

--parse a sequence of expressions
parserSeqExpr :: Parser String
parserSeqExpr = do expr <- parserExp
                   do symbol ","
                      exprs <- parserSeqExpr
                      return (expr ++ "," ++exprs)
                      <|>
                      return expr

--parse the parameters for a function
parserY :: Parser String
parserY = do symbol "("
             seq_expr <- parserSeqExpr
             symbol ")"
             return ("(" ++ seq_expr ++ ")")
          <|>
          do symbol "("
             symbol ")"
             return "()"

parserExp :: Parser String
parserExp = do prog <- parserProg
               return prog
            <|> 
            do symbol "lambda"
               symbol "("
               seq_var <- parserSeqVar
               symbol ")"
               expr <- parserExp
               return ("lambda(" ++ seq_var ++ ")" ++ expr)
            <|> 
            do expa <- parserExpa
               return expa
            <|> 
            do opp <- parserOPP
               symbol "("
               seq_var <- parserSeqVar
               symbol ")"
               return (opp ++ "(" ++ seq_var ++ ")")
            <|> 
            do symbol "if"
               test <- parserExp
               symbol "then"
               ifTrue <- parserExp
               symbol "else"
               ifFalse <- parserExp
               return ("if " ++ test ++ " then " ++ ifTrue ++ " else " ++ ifFalse)

parserBind :: Parser String
parserBind = do x <- identifier
                symbol "="
                expr <- parserExp
                do symbol "and"
                   bind <- parserBind
                   return (x ++ "=" ++ expr ++ "and" ++ bind)
                   <|>
                   return (x ++ "=" ++ expr)

parserProg :: Parser String
parserProg = do symbol "let"
                bind <- parserBind
                symbol "in"
                expr <- parserExp
                symbol "end"
                return ("let " ++ bind ++ " in " ++ expr ++ " end")
             <|> 
             do symbol "letrec"
                bind <- parserBind
                symbol "in"
                expr <- parserExp
                symbol "end"
                return ("letrec " ++ bind ++ " in " ++ expr ++ " end")