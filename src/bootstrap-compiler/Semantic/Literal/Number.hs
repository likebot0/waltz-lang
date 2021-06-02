module Semantic.Literal.Number where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer

analyze :: Semantic.Analyzer.Analyze "literal/number"
analyze x = do
    pure $ Ast.Node
        do Ast.children x
        do Ast.attributes x
