module Semantic.Statement.LetStatement where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.KeyValue

analyze :: Semantic.Analyzer.Analyze "statement/let"
analyze x = do
    Ast.Node
        do Ast.attributes x
        <$> do
            Semantic.KeyValue.analyze $ Ast.children x
