import Control.Monad
import Control.Applicative
import Data.Char

-- eval "let x=2 and y=4 in x+y*2 end"
-- 
-- eval "letrec fact=lambda(n) if eq(n,1) then 1 else n* fact (n-1) and x=cons(1, cons( 2,null)) and f = lambda (l,g) if eq(l, null) then null else cons(g (head(l)), f (g, tail (l))) in f(x,fact) end"
-- LETREC (CALL (VAR "f") [VAR "x",VAR "fact"]) [(VAR "fact",LAMBDA [VAR "n"] (IF (EQ (VAR "n") (NUM 1)) (NUM 1) (MULT (VAR "n") (CALL (VAR "fact") [SUB (VAR "n") (NUM 1)])))),(VAR "x",CONS (NUM 1) (CONS (NUM 2) NULL)),(VAR "f",LAMBDA [VAR "l",VAR "g"] (IF (EQ (VAR "l") NULL) NULL (CONS (CALL (VAR "g") [H (VAR "l")]) (CALL (VAR "f") [VAR "g",T (VAR "l")]))))]


-- tree data type to represent value parsed from the given grammar
data LKC = VAR String | NUM Int | NULL | ADD LKC LKC |
           SUB LKC LKC | MULT LKC LKC | DIV LKC LKC |
           EQ LKC LKC | LEQ LKC LKC | H LKC | T LKC | CONS LKC LKC |
           IF LKC LKC LKC | LAMBDA [LKC] LKC | CALL LKC [LKC] |
           LET LKC [(LKC,LKC)] | LETREC LKC [(LKC, LKC)]
           deriving(Show, Eq)

-- parser type
newtype Parser a = P(String -> [(a, String)])

-- apply a parser to a given data
parse :: Parser a -> String -> [(a, String)]
parse (P parser) input = parser input

-- parser is a functor -> defined because of monad is needed
instance Functor Parser where
    -- (a -> b) -> Parser a -> Parser b
    fmap f parser = P(\input -> case parse parser input of 
                                    [] -> []
                                    [(v,other)] -> [(f v,other)])

-- parser is an applicative -> defined because of monad is needed
instance Applicative Parser where
    -- a -> Parser a
    pure v = P(\input -> [(v,input)])

    -- Parser (a -> b) -> Parser a -> Parser b
    parserf <*> parserx = P(\input -> case parse parserf input of 
                                        [] -> []
                                        [(f,other)] -> parse (fmap f parserx) other)

-- parser is a monad
instance Monad Parser where
    -- a -> Parser a
    return = pure

    -- Parser a -> (a -> Parser b) -> Parser b
    parser >>= f = P(\input -> case parse parser input of 
                                [] -> []
                                [(v,other)] -> parse (f v) other)

-- parser is an alternetive -> used to apply the rules of the grammar and deal with the fact that the rule applied could be the wrong one automatically
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

-- parser that extract the first character of a string and if it doesn't fail return a list with a couple with that char and the rest of the string
-- e.g. parse item "ciao" -> [('c', "iao")]
--      parse item "" -> []
item :: Parser Char
item = P(\input -> case input of 
                        [] -> []
                        (x:xs) -> [(x,xs)])

-- function that given a predicate return a parser if the predicate is true
-- e.g. parse (sat (=='c')) "ciao" -> [('c',"iao")]
--      parse (sat (=='c')) "salve" -> []
sat :: (Char -> Bool) -> Parser Char
sat predicate = do x <- item
                   if (predicate x) 
                       then return x
                       else empty   

-- check if the first character of a string is a digit
-- e.g. parse (digit) "1234" -> [('1',"234")]
--      parse (digit) "ciao" -> []
digit :: Parser Char
digit = sat isDigit

-- check if the first character of a string is a lower case
-- e.g. parse (lower) "ciao" -> [('c',"iao")]
--      parse (lower) "Ciao" -> []
lower :: Parser Char
lower = sat isLower

-- check if the first character of a string is a letter
-- e.g. parse (lower) "ciao" -> [('c',"iao")]
--      parse (lower) "1234" -> []
letter :: Parser Char
letter = sat isAlpha

-- check if the first character of a string is an alphanumeric character
-- e.g. parse (alphanum) "ciao" -> [('c',"iao")]
--      parse (alphanum) " ciao" -> []
alphanum :: Parser Char
alphanum = sat isAlphaNum

--check if a string start with a certain character
-- e.g. parse (char 'c') "ciao" -> [('c',"iao")]
--      parse (char 'z') "ciao" -> []
char :: Char -> Parser Char
char c = sat (== c)

--check if a string start with a certain substring
-- e.g. parse (string "ci") "ciao" -> [("ci","ao")]
--      parse (string "ma") "ciao" -> []
string :: String -> Parser String
string [] = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)

-- check if a string could be an identifier (such as in the programming languages)
-- e.g. parse identifier "c1Ao" -> [("c1Ao","")]
--      parse identifier "Ciao" -> []
--      parse identifier "2ciao" -> []
--      parse identifier "ci_ao" -> [("ci","_ao")]
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

-- parse a natural number
-- e.g. parse nat "88" -> [(88,"")]
--      parse nat "-3" -> []
--      parse nat "ciao" -> []
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- remove one or more spaces before a string
-- e.g. parse space "    ciao" -> [((),"ciao")]
--      parse space "ciao" -> [((),"ciao")]
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- parse an int number: first it tries to read a '-' sign(negative integer), if it fails it tries to read a natural number(positive integer)
-- e.g. parse int "88" -> [(88,"")]
--      parse int "-3" -> [(-3,"")]
--      parse int "ciao" -> []
int :: Parser Int
int = do char '-'
         n <- nat
         return (-1 * n)
         <|>
         nat

--given a parser, remove the spaces before and after the value
token :: Parser a -> Parser a
token parser = do space
                  value <- parser
                  space
                  return value

-- check if a string could be an identifier (such as in the programming languages) removing spaces before and after
-- e.g. parse identifier "   c1Ao   " -> [("c1Ao","")]
--      parse identifier "Ciao" -> []
--      parse identifier "2ciao" -> []
--      parse identifier "ci_ao" -> [("ci","_ao")]
identifier :: Parser String
identifier = token ident

-- parse an integer removing spaces before and after
-- e.g. parse int "   88   " -> [(88,"")]
--      parse int "   -3   " -> [(-3,"")]
--      parse int "ciao" -> []
integer :: Parser Int
integer = token int

-- recognize a given string removing spaces before and after
-- e.g. parse (string "ci") "   ci   ao" -> [("ci","ao")]
--      parse (string "ma") "ciao" -> []
symbol :: String -> Parser String
symbol xs = token (string xs)


-- *******************************************************************************************************************
-- --------------------------------------------   LKC parsers   ------------------------------------------------------
-- *******************************************************************************************************************


-- parse a sequence of variables, separated by a comma
-- e.g. parse parserSeqVar "x, y, z" -> [([VAR "x",VAR "y",VAR "z"],"")]
--      parse parserSeqVar "x" -> [([VAR "x"),""]
--      parse parserSeqVar "2x" -> []
--      parse parserSeqVar "x," -> []
parserSeqVar :: Parser [LKC]
parserSeqVar = do x <- identifier
                  do symbol ","
                     vars <- parserSeqVar
                     return ((VAR x):vars)
                     <|>
                     return [VAR x]
                  <|>
                  return []

-- parse operations cons, eq and leq with prefix operator (operation with 2 parameters)
parserOPP2 :: Parser (LKC->LKC->LKC)
parserOPP2 = do symbol "cons"
                return CONS
             <|>
             do symbol "eq"
                return Main.EQ
             <|>
             do symbol "leq"
                return LEQ

-- parse operations head and tail with prefix operator (operation with 1 parameters)
parserOPP1 :: Parser (LKC->LKC)
parserOPP1 = do symbol "head"
                return H
             <|>
             do symbol "tail"
                return T

-- parse a "system" function call
-- parse parserOPP "head(1)" -> [(H (NUM 1),"")]            
-- parse parserOPP "head(1,2,3)" -> []
-- parse parserOPP "cons(1,2)" -> [(CONS (NUM 1) (NUM 2),"")]
parserOPP :: Parser LKC
parserOPP = do opp2 <- parserOPP2
               symbol "("
               expr1 <- parserExp
               symbol ","
               expr2 <- parserExp
               symbol ")"
               return (opp2 expr1 expr2)
               -- parse a function with 2 parameters
            <|> 
            do opp1 <- parserOPP1
               symbol "("
               expr1 <- parserExp
               symbol ")"
               return (opp1 expr1)
               -- parse a function with 1 parameter

--parse a multiplicative operation (*,/) with infix operator
parserOPM :: Parser (LKC -> LKC -> LKC)
parserOPM = do symbol "*"
               return MULT
            <|>
            do symbol "/"
               return DIV

--parse an aritmetic operation(+,-) with infix operator
parserOPA :: Parser (LKC -> LKC -> LKC)
parserOPA = do symbol "+"
               return ADD
            <|>
            do symbol "-"
               return SUB

-- parse a string that contains an aritmetic operation
-- e.g. parse parserExpa "x+y" -> [(ADD (VAR "x") (VAR "y"),"")]
parserExpa :: Parser LKC
parserExpa = do term <- parserTerm
                do opa <- parserOPA
                   expa <- parserExpa
                   return (opa term expa)
                   <|>
                   return term

-- parse a string that rapresent a factor (null, variables, function calls, integers or an in brackets aritmetic operation)
-- e.g. parse parserFactor "null" -> [(NULL,"")]
--      parse parserFactor "x" -> [(VAR "x","")]
--      parse parserFactor "fun(2)" -> [(CALL (VAR "fun") [NUM 2],"")]
--      parse parserFactor "-2" -> [(NUM (-2),"")]
--      parse parserFactor "(x+y)" -> [(ADD (VAR "x") (VAR "y"),"")]
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
               do num <- integer
                  return (NUM num)
               <|>
               do symbol "("
                  expa <- parserExpa
                  symbol ")"
                  return (expa)

-- parse a string that rapresent a term (a factor or a multiplicative operation between terms)
-- e.g. parse parserTerm "(x+y)*2" -> [(MULT (ADD (VAR "x") (VAR "y")) (NUM 2),"")]
parserTerm :: Parser LKC
parserTerm = do factor <- parserFactor
                do opm <- parserOPM
                   term <- parserTerm
                   return (opm factor term)
                   <|>
                   return (factor)

--parse the parameters for a function if there is, otherwise it parse the empty brackets
-- e.g. parse parserY "(x,y)" -> [([VAR "x",VAR "y"],"")]
--      parse parserY "()" -> [([],"")]
parserY :: Parser [LKC]
parserY = do symbol "("
             do seq_expr <- parserSeqExpr
                symbol ")"
                return (seq_expr)
                <|>
                do symbol ")"
                   return []

-- parse dichiarations(one or more) of variables that are before the bosy of the program
-- e.g. parse parserBind  "x = 7 and y = x and z = lambda (n) n*2" ->
--          [([(VAR "x",NUM 7),(VAR "y",VAR "x"),(VAR "z",LAMBDA [VAR "n"] (MULT (VAR "n") (NUM 2)))],"")]
parserBind :: Parser [(LKC, LKC)]
parserBind = do x <- identifier
                symbol "="
                expr <- parserExp
                do symbol "and"
                   bind <- parserBind
                   return ((VAR x, expr) : bind)
                   <|>
                   return [(VAR x, expr)]

-- parse an lkc program
-- e.g. parse parserProg "let x=2 and y=4 in x+y*2 end" -> 
--          [(LET (ADD (VAR "x") (MULT (VAR "y") (NUM 2))) [(VAR "x",NUM 2),(VAR "y",NUM 4)],"")]
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
-- parse an if-then-else construct
-- parse parserIf "if eq(x,y) then x else y" -> [(IF (EQ (VAR "x") (VAR "y")) (VAR "x") (VAR "y"),"")]
parserIf :: Parser LKC
parserIf = do symbol "if"
              test <- parserExp
              symbol "then"
              ifTrue <- parserExp
              symbol "else"
              ifFalse <- parserExp
              return (IF test ifTrue ifFalse)

-- parse a lambda function
-- parse parserLambda "lambda (n) n*2" -> [(LAMBDA [VAR "n"] (MULT (VAR "n") (NUM 2)),"")]
parserLambda :: Parser LKC
parserLambda = do symbol "lambda"
                  parameters <- parserY
                  expr <- parserExp
                  return (LAMBDA parameters expr)

-- parse an lkc expression
parserExp :: Parser LKC
parserExp = do lambda <- parserLambda
               return lambda
               -- parse a lambda expression
            <|>
            do fun <- parserOPP
               return fun
               -- parse a "system" function call
            <|> 
            do prog <- parserProg
               return prog
               -- parse an lkc program
            <|>
            do ifConstr <- parserIf
               return ifConstr
               -- parse an if-then-else construct
            <|> 
            do expa <- parserExpa
               return expa
               -- parse an aritmetic expression

-- parse a sequence of expressions divided by a comma
-- e.g. parse parserSeqExpr "x+y, x*y, fun(2), let x=2 in f(x) end" -> 
--         [([ADD (VAR "x") (VAR "y"),MULT (VAR "x") (VAR "y"),CALL (VAR "fun") [NUM 2],LET (CALL (VAR "f") [VAR "x"]) [(VAR "x",NUM 2)]],"")]
parserSeqExpr :: Parser [LKC]
parserSeqExpr = do expr <- parserExp
                   do symbol ","
                      exprs <- parserSeqExpr
                      return (expr:exprs)
                      <|>
                      return ([expr])

-- evaluate a lkc expression. If it is correct return an lkc tree, if it is correct within a certain point gives an error and return the non parsed part of the expression, if it is wrong return a generic error
eval :: String -> LKC
eval s = case (parse parserExp s) of 
              [(lkc,[])] -> lkc
              [(_,out)] -> error ("unused input: " ++ out)
              [] -> error ("invalid input")