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
  | Bool Bool

test = [1]

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString =
  do char '"'
     x <- many (noneOf "\"")
     char '"'
     return $ String x
     
parseAtom =
  do first <- letter <|> symbol
     rest <- many (letter <|> digit <|> symbol)
     let atom = first:rest
     return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom
                
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main =
  do (expr:_) <- getArgs
     putStrLn (readExpr expr) 
