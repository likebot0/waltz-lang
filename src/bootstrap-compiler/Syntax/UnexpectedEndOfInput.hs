module Syntax.UnexpectedEndOfInput where

import Global
import Context.Funs
import Text.Megaparsec
import qualified Ast.Syntax
import qualified Diagnostic
import qualified Syntax.Analyzer
import qualified Syntax.Diagnostic.Position

analyzer :: Syntax.Analyzer.Analyzer ()
analyzer = () <$ do
    position <- Syntax.Diagnostic.Position.analyzer 

    eof

    lift do
        call @ "diagnostic/send" do
            Diagnostic.Diagnostic
                do inject $ Proxy @ "error"
                do Diagnostic.Location position position
                do "Syntax error, unexpected end of input"
