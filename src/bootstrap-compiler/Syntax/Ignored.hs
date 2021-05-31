module Syntax.Ignored where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Comment
import qualified Syntax.Whitespace
import qualified Syntax.UnexpectedToken

analyzer :: Syntax.Analyzer.Analyzer ()
analyzer = choice
    [ Syntax.Comment.analyzer
    , Syntax.Whitespace.analyzer
    , Syntax.UnexpectedToken.analyzer
    ]
