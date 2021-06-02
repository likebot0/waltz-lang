module Syntax.Literal.String where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.GroupedExpression
import qualified Syntax.Common
import qualified Syntax.UnexpectedEndOfInput
import qualified Syntax.UnexpectedToken

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "literal/string")
analyzer = Syntax.Common.node do
    Syntax.Common.between
        do single '"'
        do single '"'
        do choice
            [ inject <$> some 
                do choice
                    [ noneOf "\\"
                    , charEscape
                    ]
            , inject <$> interpolation
            ]
        do
            single '\\'
            choice
                [ Syntax.UnexpectedEndOfInput.analyzer
                , Syntax.UnexpectedToken.analyzer
                ]

charEscape :: Syntax.Analyzer.Analyzer Char
charEscape = try do
    single '\\'

    choice
        [ '\\' <$ single '\\'
        , '\"' <$ single '"'
        , '\t' <$ single 't'
        , '\r' <$ single 'r'
        , '\n' <$ single 'n'
        ]

interpolation :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "grouped-expression")
interpolation = do
    try do
        single '\\'
        lookAhead $ single '('

    Syntax.GroupedExpression.analyzer
