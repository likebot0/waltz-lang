module Syntax.UnexpectedToken where

import Global
import Context.Funs
import Text.Megaparsec
import qualified Ast.Syntax
import qualified Diagnostic
import qualified Syntax.Analyzer
import qualified Syntax.Diagnostic.Position

analyzer :: Syntax.Analyzer.Analyzer ()
analyzer = () <$ do
    start <- Syntax.Diagnostic.Position.analyzer 

    c <- anySingle

    end <- Syntax.Diagnostic.Position.analyzer 

    lift do
        call @ "diagnostic/send" do
            Diagnostic.Diagnostic
                do liftUnion $ Proxy @ "error"
                do Diagnostic.Location start end
                do "Syntax error, unexpected token '" ++ [c] ++ "'"
