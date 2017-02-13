{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : $Header$
-- Description : The core language parser
-- Copyright   : (c) Marek Kidon, 2017
-- License     : GPL-3
-- Maintainer  : marek.kidon@gmail.com
-- Stability   : experimental
-- Portability:  GHC specific language extensions.
--
-- This module contains the core language parsec parser.
module GMachine.Core.Parser
    ( parseCoreProgram )
  where

import Data.Word (Word32)
import Data.Functor.Identity (Identity)

import Text.Parsec
    ( (<|>)
    , many
    , many1
    , try
    , unexpected
    )
import Text.Parsec.Expr
    ( Assoc(AssocLeft, AssocNone, AssocRight)
    , Operator(Infix)
    , OperatorTable
    , buildExpressionParser
    )
import Text.Parsec.String (Parser)

import GMachine.Core.Lexer
    ( m_angles
    , m_braces
    , m_comma
    , m_identifier
    , m_integer
    , m_parens
    , m_reserved
    , m_reservedOp
    , m_semiSep
    , m_semiSep1
    , m_whiteSpace
    )
import GMachine.Type.Core
    ( Alternative(Alternative)
    , CoreAlternatives
    , CoreExpr
    , CoreLocalDefinitions
    , CoreProgram
    , CoreSupercombinator
    , Expr(..)
    , LocalDefinition(LocalDefinition)
    , Supercombinator(Supercombinator)
    )


parseCoreProgram :: Parser CoreProgram
parseCoreProgram = m_semiSep1 parseScDefn

parseScDefn :: Parser CoreSupercombinator
parseScDefn = Supercombinator
    <$> m_identifier
    <*> many m_identifier <* m_reserved "="
    <*> parseExpr

parseExpr :: Parser CoreExpr
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

parseLocalDefinitions :: Parser CoreLocalDefinitions
parseLocalDefinitions = m_semiSep $ LocalDefinition
    <$> m_identifier <* m_reserved "="
    <*> parseExpr

parseAlternatives :: Parser CoreAlternatives
parseAlternatives = m_semiSep1 $ Alternative
    <$> m_angles word
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

operatorParser :: Parser CoreExpr
operatorParser =
    buildExpressionParser operatorTable parseAExpr

operatorTable :: OperatorTable String u Identity CoreExpr
operatorTable =
    [ [ Infix (m_whiteSpace >> return EAp) AssocLeft
      ]
    , [ Infix (reservedOp' "*" >>= return . binaryOperator) AssocRight
      , Infix (reservedOp' "/" >>= return . binaryOperator) AssocNone
      ]
    , [ Infix (reservedOp' "+" >>= return . binaryOperator) AssocRight
      , Infix (reservedOp' "-" >>= return . binaryOperator) AssocNone
      ]
    , [ Infix (reservedOp' "==" >>= return . binaryOperator) AssocNone
      ]
    ]
  where
    reservedOp' x = m_reservedOp x *> return x
    binaryOperator op x y = EVar op `EAp` x `EAp` y

word :: Parser Word32
word = m_integer >>= word'
  where
    word' integer
        | (integer < 0 && integer >= 4294967295) = fromIntegral <$> pure integer
        | True = unexpected "integer"
