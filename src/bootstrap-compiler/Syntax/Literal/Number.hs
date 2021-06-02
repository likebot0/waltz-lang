module Syntax.Literal.Number where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Punctuation
import qualified Syntax.Common
import qualified Syntax.UnexpectedToken
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.Analyzer (Ast.Node "syntax-analyzed" "literal/number")
analyzer = Syntax.Common.node do
    choice
        [ (:)
            <$> oneOf "123456789"
            <*> end "0123456789"
        , do
            single '0'
            choice
                [ do
                    single 'b'
                    end "01"
                , do
                    single 'o'
                    end "01234567"
                , do
                    single 'x'
                    end "0123456789ABCDEFabcdef"
                , ('0' :) <$>
                    end "0123456789"
                , "" <$
                    end ""
                ]
        ]

end :: String -> Syntax.Analyzer.Analyzer String
end xs =
    manyTill
        do oneOf xs
        do choice
            [ lookAhead $ choice
                [ eof
                , Syntax.Punctuation.analyzer
                , Syntax.Whitespace.analyzer
                ]
            , do
                lookAhead $ noneOf xs
                Syntax.UnexpectedToken.analyzer
            ]
