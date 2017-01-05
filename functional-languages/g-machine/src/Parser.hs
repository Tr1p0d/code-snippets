{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Data.Functor.Identity

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

import Core
import Lexer


parseCoreProgram :: Parser CoreProgram
parseCoreProgram = m_semiSep1 parseScDefn

parseScDefn :: Parser CoreScDefn
parseScDefn = (,,)
    <$> m_identifier
    <*> many m_identifier <* m_reserved "="
    <*> parseExpr

parseExpr =
    (try $ parseLet' True)
    <|> parseLet' False
    <|> parseCase
    <|> parseLambda
    <|> operatorParser
  where
    parseLambda = m_reserved "\\" >> ELam
        <$> many1 m_identifier <* m_reserved "."
        <*> parseExpr

    parseCase = m_reserved "case" >> ECase
        <$> parseExpr <* m_reserved "of"
        <*> parseAlternatives

    parseLet' recursive = m_reserved str >> ELet recursive
        <$> parseLocalDefinitions <* m_reserved "in"
        <*> parseExpr
      where
        str
            | recursive = "letrec"
            | otherwise = "let"

parseLocalDefinitions :: Parser [(Name, CoreExpr)]
parseLocalDefinitions = m_semiSep $ (,)
    <$> m_identifier <* m_reserved "="
    <*> parseExpr

parseAlternatives :: Parser [CoreAlt]
parseAlternatives = m_semiSep1 $ (,,)
    <$> m_angles m_integer
    <*> many m_identifier <* m_reserved "->"
    <*> parseExpr

parseAExpr :: Parser CoreExpr
parseAExpr =
    parsePack
    <|> EVar <$> m_identifier
    <|> ENum <$> m_integer
    <|> m_parens parseExpr
  where
    parsePack = do
        m_reserved "Pack"
        (tag, arity) <- m_braces parseConstr
        args <- many parseAExpr
        return $ EConstr tag arity args
      where
        parseConstr = (,) <$> m_integer <* m_comma <*> m_integer

operatorParser =
    buildExpressionParser operatorTable parseAExpr
  where
    parseApplication = do
        (spineTop:rest) <- many1 parseAExpr
        return $ foldl EAp spineTop rest

operatorTable :: OperatorTable String u Identity CoreExpr
operatorTable =
    [ [ Infix (reservedOp' "*" >>= return . binaryOperator) AssocRight
      ]
    , [ Infix (reservedOp' "+" >>= return . binaryOperator) AssocRight
      ]
    , [ Infix (m_whiteSpace >> return EAp) AssocLeft
      ]
    ]
  where
    reservedOp' x = m_reservedOp x *> return x
    binaryOperator op x y = EVar op `EAp` x `EAp` y
