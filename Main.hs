module Main where
import Text.ParserCombinators.Parsec hiding (spaces, many, (<|>))
import Control.Monad
import Control.Applicative
import System.Environment

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Char Char
  | Bool Bool
  deriving (Show)

test = [1]

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escaped :: Char -> Char -> Parser Char
escaped esc as = try $ string ('\\':esc:[]) >> return as

parseChar :: Parser LispVal
parseChar = string "#\\" >> anyChar >>= (return . Char)

parseString :: Parser LispVal
parseString =
  do char '"'
     x <- many (escaped 'n' '\n' <|> escaped '"' '"' <|> noneOf "\"")
     char '"'
     return $ String x
     
parseAtom =
  do first <- try letter <|> symbol
     rest <- many (letter <|> digit <|> symbol)
     let atom = first:rest
     return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom
                
parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseParens :: Parser LispVal
parseParens = do char '('
                 x <- try parseList <|> parseDottedList
                 char ')'
                 return x

parseDottedList :: Parser LispVal
parseDottedList =
  do head <- parseExpr `endBy` spaces
     tail <- char '.' >> spaces >> parseExpr
     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted =
  do char '\''
     x <- parseExpr
     return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseChar
         <|> parseQuoted
         <|> parseNumber
         <|> parseParens

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val

main :: IO ()
main =
  do (expr:_) <- getArgs
     putStrLn (readExpr expr) 
