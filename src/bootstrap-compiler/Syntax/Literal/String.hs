module Syntax.Literal.String where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.GroupedExpression
import qualified Syntax.Shared
import qualified Syntax.UnexpectedToken

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "literal/string")
analyzer = Syntax.Shared.node do
    Syntax.Shared.between
        do single '"'
        do single '"'
        do choice
            [ liftUnion <$> some 
                do choice
                    [ noneOf "\\"
                    , escape
                    ]
            , liftUnion <$> placeholder
            ]
        do
            single '\\'
            choice
                [ eof
                , Syntax.UnexpectedToken.analyzer
                ]

escape :: Syntax.Analyzer.Analyzer Char
escape = try do
    single '\\'

    choice
        [ '\t' <$ single 't'
        , '\r' <$ single 'r'
        , '\n' <$ single 'n'
        , '\"' <$ single '"'
        ]

placeholder :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "grouped-expression")
placeholder = do
    try do
        single '\\'
        lookAhead $ single '('

    Syntax.GroupedExpression.analyzer
