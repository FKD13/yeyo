module LanguageParser where

import Parser
import Types

import Data.Char
import Control.Applicative

-----------------------------
digit :: Parser Char
digit = spot isDigit

letter :: Parser Char
letter = spot isAlpha

variableName :: Parser String
variableName = some $ digit <|> letter

eol :: Parser String
eol = exact "\n" <|> exact "\r\n" <|> exact "\r"

space :: Parser Char
space = token '\t' <|> token ' '

whitespace :: Parser String
whitespace = some space

opt :: Parser String -> Parser String
opt = (<|>) (return "")

separation :: Parser String
separation = ((opt whitespace >> eol) <|> (whitespace >> opt eol)) >> separation

encapsulate :: Parser a -> Parser a
encapsulate a = do _ <- token '(' >> opt whitespace
                   b <- a
                   _ <- opt whitespace  >> token ')'
                   return b
----------------------------

parseProgram :: Parser Program
parseProgram = do _ <- token '[' >> opt whitespace >> opt eol >> opt whitespace
                  e <- many parseExpression
                  e2 <- parseLastExpression
                  _ <- opt whitespace >> opt eol >> opt whitespace >> token ']' >> opt whitespace >> opt eol >> opt whitespace
                  return $ e++[e2]

parseExpression :: Parser Expression
parseExpression = do e <- parseSingleExpression
                     _ <- opt whitespace >> token ',' >> opt whitespace >> opt eol >> opt whitespace
                     return e

parseLastExpression :: Parser Expression
parseLastExpression = parseSingleExpression <|> return EmptyExpr

parseSingleExpression :: Parser Expression
parseSingleExpression = parseSet <|> parseUnset <|> parseAsk <|> parseSay

parseSet :: Parser Expression
parseSet = do _ <- exact "SET" >> whitespace
              v <- parseVariable
              SetExpr v <$> parseValue

parseUnset :: Parser Expression
parseUnset = do _ <- exact "UNSET" >> whitespace
                UnsetExpr <$> parseVariable

parseSay :: Parser Expression
parseSay = do _ <- exact "SAY" >> whitespace
              SayExpr <$> parseValue

parseAsk :: Parser Expression
parseAsk = do _ <- exact "ASK" >> whitespace
              AskExpr <$> parseVariable

parseIf :: Parser Expression
parseIf = do _ <- exact "IF" >> whitespace
             e <- parseValue
             p1 <- parseProgram
             _ <- token '-'
             IfExpr e p1 <$> parseProgram

parseVariable :: Parser Value
parseVariable = do _ <- token '<'
                   n <- variableName
                   _ <- token '>'
                   return $ VarValue n

parseValue :: Parser Value
parseValue = parseVariable <|> 
             encapsulate parseAddValue <|> 
             encapsulate parseSubValue <|> 
             encapsulate parseMultValue <|>
             parseNumValue <|>
             parseStringValue

parseAddValue :: Parser Value
parseAddValue = do v1 <- parseValue
                   _ <- opt whitespace >> token '+' >> opt whitespace
                   AddValue v1 <$> parseValue

parseSubValue :: Parser Value
parseSubValue = do v1 <- parseValue
                   _ <- opt whitespace >> token '-' >> opt whitespace
                   SubValue v1 <$> parseValue

parseMultValue :: Parser Value
parseMultValue = do v1 <- parseValue
                    _ <- opt whitespace >> token '*' >> opt whitespace
                    MultValue v1 <$> parseValue

parseNumValue :: Parser Value
parseNumValue = do d <- some digit
                   return $ NumValue (read d :: Int)

parseStringValue :: Parser Value
parseStringValue = do _ <- token '"'
                      s <- many one
                      _ <- token '"'
                      return $ StringValue s
