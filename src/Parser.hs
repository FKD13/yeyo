module Parser(Parser, one, token, spot, star, plus, get, exact, parse) where

import Control.Monad
import Control.Applicative 

newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser $ \s -> [(x, s)]
  m >>= k  = Parser $ \s -> [(w, v) | (x, y) <- parse m s, (w, v) <- parse (k x) y]

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  mzero     = Parser $ const []
  mplus a b = Parser $ \s -> parse a s ++ parse b s

-----------------------------------------------------------------------------------

one :: Parser Char
one = Parser f
  where
    f [] = []
    f (c:s) = [(c, s)]

token :: Char -> Parser Char
token c = do chr <- one
             guard $ c == chr
             return chr

spot :: (Char -> Bool) -> Parser Char
spot f = do c <- one
            guard(f c)
            return c

star :: Char -> Parser String
star = many . token

plus :: Char -> Parser String
plus = some . token

get :: Int -> Parser String
get 0 = return ""
get n = do s <- one
           s' <- get $ n-1
           return $ s:s'

exact :: String -> Parser String
exact s = do s' <- get $ length s
             guard(s == s')
             return s'
