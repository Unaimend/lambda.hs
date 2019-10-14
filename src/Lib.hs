module Lib where

import Text.ParserCombinators.Parsec hiding (spaces)
import Prelude hiding(lookup)

data Term = Var String
          | Number Int
          | Lam Term Term
          | App Term Term deriving(Show)

data Value = Num Int | Fun (Value -> Value) | Wrong
type Environment = [(String, Value )]

instance Show Value where
  show = showval

toString :: Term -> String
toString (Var name) = name
toString _ = "WRONG"

showval :: Value -> String
--showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

lookup :: String -> Environment -> Value
lookup x [] = Wrong
lookup x ((y,b):e) = if x==y then b else lookup x e

eval :: Term -> Environment -> Value
eval (Var x) e = lookup x e
eval (Number x) _ = Num x
eval (Lam var body) e = Fun (\a -> (eval body) ((toString var,a):e))
eval (App lam arg) e = apply (eval lam e) (eval arg e)

apply :: Value -> Value ->  Value
apply (Fun k) a = k a
apply f a = Wrong

spaces :: Parser ()
spaces = skipMany space

parseOpen :: Parser ()
parseOpen = do
  _ <- spaces
  _ <- string "("
  _ <- spaces
  return ()

parseClosed :: Parser ()
parseClosed = do
  _ <- spaces
  _ <- string ")"
  _ <- spaces
  return ()

parseLam :: Parser Term
parseLam = do
  _ <- parseOpen
  _ <- string "\\"
  var <- parseVar
  _ <- string "."
  body <- term 
  _ <- parseClosed
  return $ Lam var body


parseNumber :: Parser Term
parseNumber = Number . read <$> many1 digit

parseVar :: Parser Term
parseVar = do
  val <- anyChar
  return $ Var [val]

term :: Parser Term
term = do
  _ <- spaces
  parseNumber
  <|> parseLam
  <|> parseVar

expr :: Parser Term
expr = do
  es <- many term
  return (foldl1 App es)


readExpr :: String -> Term
readExpr input = case parse expr "untyped" input of
  Left err -> Var $ (show  err)
  Right val -> val

