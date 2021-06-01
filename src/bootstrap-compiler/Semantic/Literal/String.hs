module Semantic.Literal.String where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer

analyze :: Semantic.Analyzer.Analyze "literal/string"
analyze x = do
    pure $ Ast.Node
        do Ast.attributes x
        do [ inject "" ]
