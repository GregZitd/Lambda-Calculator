module LambdaParser where

import LambdaTypes

import Data.List (intersperse)

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser,parseFromFile)
import Text.Parsec (try, parse)
import Text.Parsec.Char (anyChar, oneOf, char, digit, satisfy, alphaNum, noneOf)
import Text.Parsec.Combinator (many1, choice, chainl1, eof)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit, isLower, isUpper)


string :: String -> Parser String
string xs = mapM char xs

parseSpace :: Parser ()
parseSpace = void $ many $ (oneOf " \n")

lexeme :: Parser a -> Parser a
lexeme p = p <* parseSpace

parseVarName :: Parser Variable
parseVarName = do
  fc <- satisfy (\x -> isDigit x || isLower x) <|> oneOf "+-*/"
  rest <- lexeme $ many (alphaNum <|> oneOf "+-*/")
  return (fc : rest)

parseVar :: Parser Lterm
parseVar = Var <$> parseVarName

parsePrant :: Parser a -> Parser a
parsePrant p = do
  lexeme $ char '('
  x <- p
  lexeme $ char ')'
  return x


parseContextSymbol :: Parser Lterm
parseContextSymbol = string "[ ]" >> return Context

--Parses an abstraction, empty contexts allowed
parseAbs :: Parser Lterm
parseAbs = do
  lexeme $ string "lambda"
  vars <- many1 $ lexeme $ parseVarName
  lexeme $ char '.'
  body <- lexeme $ parseApp
  return (desugarAbs vars body)

--calling it on an empty list should return some kind of error instead of t
--a solution would be to make a wrapper around Parser that can return other
--kinds of errors, not just ParseError
desugarAbs :: [Variable] -> Lterm -> Lterm
desugarAbs [] t = t
desugarAbs (x : []) t = Abs x t
desugarAbs (x : xs) t = Abs x (desugarAbs xs t)


--parses a lambda term, empty contexts allowed
parseApp :: Parser Lterm
parseApp = chainl1 term op
  where
    term =   parseContextSymbol
         <|> parseVar
         <|> parsePrant (try parseAbs <|> parseApp)
    op = return App

--parses a lambdaterm with possible contexts added eg: [Context| ...]
--Note that if you write brackets, you have to give at least one context,
--so [| ...] is not allowed 
parseLterm :: Parser ([ContextName], Lterm)
parseLterm = parseSpace *> ((try $ withContexts parseApp) <|> (noContext parseApp))
  where noContext :: Parser Lterm -> Parser ([ContextName],Lterm)
        noContext p = (,) [] <$> p

--helper for parseLterm
withContexts :: Parser Lterm -> Parser ([ContextName],Lterm)
withContexts p = do
  lexeme $ char '['
  firstName <- lexeme parseContextName
  otherNames <- many $ (lexeme (char ',')) *> (lexeme parseContextName)
  lexeme $ char '|'
  term <- lexeme $ p
  lexeme $ char ']'
  return (firstName : otherNames, term)

parseContextName :: Parser ContextName
parseContextName = do
  fc <- satisfy (isUpper)
  rest <- many (alphaNum <|> char '\'')
  return (fc : rest)

--can parse contexts that contain other contexts, so
--Context OneTwo = [One, Two| ...] is allowed
parseContextDef :: Parser (ContextName, [ContextName], Lterm)
parseContextDef = do
  lexeme $ string "Context"
  contextName <- lexeme $ parseContextName
  lexeme $ char '='
  (usedNames,term) <- parseLterm
  return (contextName, usedNames, term)


--the problem with this is that everything that starts with a "C" and is not
--a context definition will fail.
--since variable names must not start with an uppercase letter now, this should
--not lead to unwarranted parse errors
parseRepl :: Parser ReplInput
parseRepl =
      (parseContextDef >>= return . (uncurry3 ReplContextDef))
  <|> (parseLterm >>= return . (uncurry ReplLterm))
  where uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
        uncurry3 f (a, b, c) = f a b c


runReplParser :: String -> Either Error ReplInput
runReplParser input =
  case parse parseRepl "" input of
    Left err   -> Left $ CannotParse err
    Right (ReplContextDef contextName usedNames term) ->
      ReplContextDef contextName usedNames <$> validateContextDef contextName term
    Right else' -> Right else'

--uses parseContextFile to parse the file and converts ParseError to Error
contextFile :: IO (Either Error [ReplInput])
contextFile =
  let parseResult :: IO (Either Error [(ContextName, [ContextName], Lterm)])
      parseResult = do
        result <- parseFromFile parseContextFile "Lambda-source/lambdaContexts.txt"
        case  result of
          Left err -> return $ Left (CannotParse err)
          Right ls -> return $ Right ls
   in (fmap . fmap . fmap) (uncurry3 ReplContextDef) parseResult
  where uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
        uncurry3 f (a, b, c) = f a b c




--Validating that a context definition has only one empty context in it.
validateContextDef :: String -> Lterm -> Either Error Lterm
validateContextDef name term =
  case numberOfContexts term of
    0 -> Left $ ZeroContexes name term
    1 -> Right term
    _ -> Left $ MultipleContexts name term
  where numberOfContexts Context = 1
        numberOfContexts (Var _) = 0
        numberOfContexts (Abs x y) = numberOfContexts y
        numberOfContexts (App x y) =   numberOfContexts x
                                     + numberOfContexts y
parseContextFile :: Parser [(ContextName, [ContextName], Lterm)]
parseContextFile = many $ parseContextDef <* (lexeme $ char ';')