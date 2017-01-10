module GMachine.Core.Lexer
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
  where

import Data.Monoid ((<>))

import Text.Parsec ((<|>), char, digit, letter, oneOf)
import Text.Parsec.Token
    ( GenTokenParser(TokenParser)
    , angles
    , braces
    , comma
    , commentEnd
    , commentLine
    , commentStart
    , identLetter
    , identStart
    , identifier
    , integer
    , makeTokenParser
    , nestedComments
    , opLetter
    , opStart
    , parens
    , reserved
    , reservedNames
    , reservedOp
    , reservedOpNames
    , semiSep
    , semiSep1
    , whiteSpace
    )
import Text.Parsec.Language (LanguageDef, emptyDef)


coreLanguageDef :: LanguageDef st
coreLanguageDef = emptyDef
    { commentStart = "/*"
    , commentEnd = "*/"
    , commentLine = "//"
    , nestedComments = False
    , identStart = letter
    , identLetter = letter <|> digit <|> char '_'
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
        , "Pack"
        , "->"
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
    { angles = m_angles
    , braces = m_braces
    , comma = m_comma
    , identifier = m_identifier
    , integer = m_integer
    , parens = m_parens
    , reserved = m_reserved
    , reservedOp = m_reservedOp
    , semiSep = m_semiSep
    , semiSep1 = m_semiSep1
    , whiteSpace = m_whiteSpace
    } = makeTokenParser coreLanguageDef
