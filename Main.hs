module Main where
import Text.ParserCombinators.Parsec hiding (spaces, many, (<|>))
import Control.Monad
import Control.Applicative
import System.Environment
import qualified Data.HashMap.Lazy as H
import Control.Monad.Except

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Char Char
  | Bool Bool

instance Show LispVal where
  show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show (UnboundVar message varname) =
    message ++ ": " ++ varname
  show (BadSpecialForm message form) =
    message ++ ": " ++ show form
  show (NotFunction message func) =
    message ++ ": " ++ show func
  show (NumArgs expected found) =
    "Expected " ++ show expected ++ " arguments; found value" ++ plural found ++ ": " ++ unwordsList found
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) =
    "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

plural ::  [a] -> String
plural s = if length s > 1 then "s" else ""

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
     
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseChar
         <|> parseQuoted
         <|> parseNumber
         <|> parseParens

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

primitives :: H.HashMap String ([LispVal] -> ThrowsError LispVal)
primitives = H.fromList [("+", numericBinop (+))
                        ,("-", numericBinop (-))
                        ,("*", numericBinop (*))
                        ,("/", numericBinop div)
                        ,("mod", numericBinop mod)
                        ,("quotient", numericBinop quot)
                        ,("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params
  | length params < 2 = throwError (NumArgs 2 params)
  | otherwise         = fmap (Number . foldl1 op) $ traverse unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "not found" func)
                        ($ args)
                        (H.lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@ (String _) = return val
eval val@ (Number _) = return val
eval val@ (Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

main :: IO ()
main = do
  expr <- head <$> getArgs
  result <- return $ eval =<< readExpr expr
  putStrLn . extractValue $ (show <$> result) `catchError` (return . show)
