module Parser where

import Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L 
import Control.Monad.Combinators.Expr


-- Random things
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty -- no multiline comments

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pInt :: Parser Expr 
pInt = Float <$> lexeme L.decimal

pFloat :: Parser Expr 
pFloat = Float <$> lexeme L.float 

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pIdentifier :: Parser Expr
pIdentifier = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pStr :: Parser String
pStr = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "naming conflict")

pFunction :: Parser Expr 
pFunction = do 
  lexeme $ string "def"
  xs   <- lexeme pStr 
  args <- lexeme (parens $ many pIdentifier) 
  body <- lexeme pExpr
  return $ Function xs args body

pCall :: Parser Expr
pCall = do
  funName <- lexeme pStr
  args    <- lexeme (parens $ sepBy pIdentifier (char ','))
  return $ Call funName args -- should functions accept numbers?

-- Expressions!

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , try pCall
  , pIdentifier
  , pInt
  , pFloat
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" UMinus]
  , [ binary "*" Times 
    , binary "/" Divide 
    ]
  , [ binary "+" Plus  
    , binary "-" Minus 
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

main :: Parser Expr -> Text -> Expr
main p s = case runParser p "" s of 
          (Right e) -> e
          (Left (ParseErrorBundle t s))  -> error "gah" 

ex1 :: Text 
ex1 = T.pack "(5 + 5)"



