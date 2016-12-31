import Control.Monad
import Control.Applicative
import Data.Char

-- let x=2 and y=4 in x+y*2 end
-- "letrec fact=lambda(n) if eq(n,1) then 1 else n* fact (n-1) and x=cons(1, cons( 2,null)) and f = lambda (l,g) if eq(l, null) then null else cons(g (head(l)), f (g, tail (l))) in x+y*2 end"
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
parserSeqVar :: Parser [LKC]
parserSeqVar = do x <- identifier
                  do symbol ","
                     vars <- parserSeqVar
                     return ((VAR x):vars)
                     <|>
                     return [VAR x]
                  <|>
                  return []

--parse an operation with prefix operator
parserOPP2 :: Parser (LKC->LKC->LKC)
parserOPP2 = do symbol "cons"
                return CONS
             <|>
             do symbol "eq"
                return Main.EQ
             <|>
             do symbol "leq"
                return LEQ

parserOPP1 :: Parser (LKC->LKC)
parserOPP1 = do symbol "head"
                return H
             <|>
             do symbol "tail"
                return T
            

--parse a multiplicative operation with infix operator
parserOPM :: Parser (LKC -> LKC -> LKC)
parserOPM = do symbol "*"
               return MULT
            <|>
            do symbol "/"
               return DIV

--parse a aritmetic operation with infix operator
parserOPA :: Parser (LKC -> LKC -> LKC)
parserOPA = do symbol "+"
               return ADD
            <|>
            do symbol "-"
               return SUB

parserExpa :: Parser LKC
parserExpa = do term <- parserTerm
                do opa <- parserOPA
                   expa <- parserExpa
                   return (opa term expa)
                   <|>
                   return term

parserFactor :: Parser LKC
parserFactor = do symbol "null"
                  return (NULL)
               <|>
               do x <- identifier
                  (do y <- parserY
                      return (CALL (VAR x) (y))
                   <|>
                   return (VAR x))
               <|>
               do num <- int
                  return (NUM num)
               <|>
               do symbol "("
                  expa <- parserExpa
                  symbol ")"
                  return (expa)

parserTerm :: Parser LKC
parserTerm = do factor <- parserFactor
                do opm <- parserOPM
                   term <- parserTerm
                   return (opm factor term)
                   <|>
                   return (factor)

--parse a sequence of expressions
parserSeqExpr :: Parser [LKC]
parserSeqExpr = do expr <- parserExp
                   do symbol ","
                      exprs <- parserSeqExpr
                      return (expr:exprs)
                      <|>
                      return ([expr])

--parse the parameters for a function
parserY :: Parser [LKC]
parserY = do symbol "("
             seq_expr <- parserSeqExpr
             symbol ")"
             return (seq_expr)
          <|>
          do symbol "("
             symbol ")"
             return []

parserExp :: Parser LKC
parserExp = do symbol "lambda"
               symbol "("
               seq_var <- parserSeqVar
               symbol ")"
               expr <- parserExp
               return (LAMBDA seq_var expr)
            <|>
            do opp2 <- parserOPP2
               symbol "("
               expr1 <- parserExp
               symbol ","
               expr2 <- parserExp
               symbol ")"
               return (opp2 expr1 expr2)
            <|> 
            do opp1 <- parserOPP1
               symbol "("
               expr1 <- parserExp
               symbol ")"
               return (opp1 expr1)
            <|> 
            do prog <- parserProg
               return prog
            <|>
            do symbol "if"
               test <- parserExp
               symbol "then"
               ifTrue <- parserExp
               symbol "else"
               ifFalse <- parserExp
               return (IF test ifTrue ifFalse)
            <|> 
            do expa <- parserExpa
               return expa

parserBind :: Parser [(LKC, LKC)]
parserBind = do x <- identifier
                symbol "="
                expr <- parserExp
                do symbol "and"
                   bind <- parserBind
                   return ((VAR x, expr) : bind)
                   <|>
                   return [(VAR x, expr)]

parserProg :: Parser LKC
parserProg = do symbol "let"
                bind <- parserBind
                symbol "in"
                expr <- parserExp
                symbol "end"
                return (LET expr bind)
             <|> 
             do symbol "letrec"
                bind <- parserBind
                symbol "in"
                expr <- parserExp
                symbol "end"
                return (LETREC expr bind)

eval :: String -> LKC
eval s = case (parse parserExp s) of 
              [(lkc,[])] -> lkc
              [(_,out)] -> error ("unused input: " ++ out)
              [] -> error ("invalid input")