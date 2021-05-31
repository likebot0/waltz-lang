module Syntax.Comment where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Shared

analyzer :: Syntax.Analyzer.Analyzer ()
analyzer = () <$ do
    Syntax.Shared.between
        do single '#'
        do choice
            [ eof
            , () <$ do
                single '\n'
            , () <$ do
                single '\r'
                optional $ single '\n'
            ]
        do anySingle
        do empty
