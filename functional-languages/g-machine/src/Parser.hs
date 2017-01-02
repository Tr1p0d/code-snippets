{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Core
import Lexer


parseExpr = parseAExpr

parseAExpr :: Parser CoreExpr
parseAExpr =
    parsePack
    <|> EVar <$> m_identifier
    <|> ENum <$> m_integer
    <|> m_parens parseExpr
  where
    parsePack = do
        m_lexeme (string "Pack")
        (tag, arity) <- m_braces parseConstr
        args <- many parseExpr
        return $ EConstr tag arity args
      where
        parseConstr = (,) <$> (m_integer <* m_comma) <*> m_integer
