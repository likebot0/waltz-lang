module Semantic.Root where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.Expression
import qualified Semantic.Statement.BaseStatement

analyze :: Semantic.Analyzer.Analyze "root"
analyze x = do
    Ast.Node
        <$> (`mapM` Ast.children x) (
                do \(x :: Ast.Node "syntax-analyzed" "expression") ->
                    inject <$> Semantic.Expression.analyze x
                @>
                do \(x :: Ast.Node "syntax-analyzed" "statement/base") ->
                    inject <$> Semantic.Statement.BaseStatement.analyze x
                @>
                typesExhausted
            )
        <*> do pure $ Ast.attributes x
