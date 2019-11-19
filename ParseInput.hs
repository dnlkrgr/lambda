module ParseInput (lambdaInteract) where

import           Data.Char
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Types
import Lambda

type Parser = Parsec Void String

lambdaInteract :: String -> IO String
lambdaInteract input = do
    case parseLambdaExpr input of
        Left err -> pure $ "Parse error at offset: " ++ show (pstateOffset $ bundlePosState err)
        Right expr -> show . eval <$> runRename expr

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = lexeme L.decimal

identifier :: Parser String
identifier = (lexeme . try) p
    where p = (:) <$> letterChar <*> many alphaNumChar

number :: Parser Term
number = Num <$> integer

variable :: Parser Term
variable = Var <$> identifier

lambda :: Parser Term
lambda = do
    symbol "/"
    x <- identifier
    symbol "."
    t <- term
    pure $ Lam x t

application :: Parser Term
application = do
    symbol "$"
    t1 <- try term <|> parens term
    t2 <- try term <|> parens term
    pure $ App t1 t2

term :: Parser Term
term =
    try number <|> try variable <|> lambda <|> application

parseLambdaExpr :: String -> Either (ParseErrorBundle String Void) Term
parseLambdaExpr = parse term ""