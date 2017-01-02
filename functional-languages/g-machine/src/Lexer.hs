module Lexer where

import Data.Monoid

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language


coreLanguageDef :: LanguageDef st
coreLanguageDef = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = False
    , identStart = alphaNum
    , identLetter = alphaNum <|> digit <|> char '_'
    , opStart = oneOf "=+-*/<>&|"
    , opLetter = oneOf "=+-*/<>&|"
    , reservedOpNames = "=" : arithmetics <> relations <> booleans
    , reservedNames = names
    }
  where
    names =
        [ "let"
        , "letrec"
        , "in"
        , "case"
        , "of"
        , "\\"
        , "."
        ]
    arithmetics =
        [ "+"
        , "-"
        , "*"
        , "/"
        ]
    relations =
        [ "<"
        , "<="
        , ">"
        , ">="
        , "=="
        , "~="
        ]
    booleans =
        [ "&"
        , "|"
        ]

TokenParser
    { parens = m_parens
    , braces = m_braces
    , identifier = m_identifier
    , commaSep1 = m_commaSep1
    , commaSep = m_commaSep
    , semi = m_semi
    , reservedOp = m_reservedOp
    , reserved = m_reserved
    , semiSep = m_semiSep
    , whiteSpace = m_whiteSpace
    , integer = m_integer
    , stringLiteral = m_stringLit
    , charLiteral = m_charLit
    , lexeme = m_lexeme
    , comma = m_comma
    } = makeTokenParser coreLanguageDef
