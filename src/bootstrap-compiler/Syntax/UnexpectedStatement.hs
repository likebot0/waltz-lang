module Syntax.UnexpectedStatement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Common
import qualified Syntax.UnexpectedEndOfInput
import qualified Syntax.UnexpectedToken

analyzer :: Syntax.Analyzer.Analyzer ()
analyzer = do
    single '\\'

    choice
        [ Syntax.UnexpectedEndOfInput.analyzer
        , Syntax.UnexpectedToken.analyzer
        ]

    skipMany
        do choice
            [ eof
            , () <$ do lookAhead $ oneOf " \t#\r\n"
            ]
