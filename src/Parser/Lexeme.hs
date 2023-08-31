module Parser.Lexeme
  ( lexeme
  , parens
  , operator
  , identifier
  , natural
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (void)
import Text.Parsec

-- | A general lexeme. Skips all leading whitespaces.
lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme parser = do
  result <- parser
  spaces
  return result

-- | Parenthesize a parser.
parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between lparen rparen
  where
    lparen = lexeme . void $ char '('
    rparen = lexeme . void $ char ')'

-- | An operator helper for use in an operator table.
--
-- Returns 'a' if any of the passed strings matched.
operator :: Stream s m Char => a -> [String] -> ParsecT s u m a
operator out ops = lexeme $ do
  _ <- choice $ map (try . string) ops
  return out

-- | An identifier parser.
identifier :: Stream s m Char => ParsecT s u m Text
identifier = Text.pack <$> go
  where
    go = lexeme $ do
      head' <- letter
      tail' <- many $ alphaNum <|> char '_' <|> char '\''
      return $ head':tail'

natural :: Stream s m Char => ParsecT s u m Int
natural = lexeme $ do 
  n <- many1 digit
  return $ read n
