module Semantic.Root where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.Expression
import qualified Semantic.Root.Statement.Base

analyze :: Semantic.Analyzer.Analyze "root"
analyze x = do
    Ast.Node
        do Ast.attributes x
        <$> sequence do
            Ast.children x >>=
                do \(x :: Ast.Node "syntax-analyzed" "expression") -> do
                    pure $ inject <$> Semantic.Expression.analyze x
                @>
                do \(x :: Ast.Node "syntax-analyzed" "root/statement/base") -> do
                    pure $ inject <$> Semantic.Root.Statement.Base.analyze x
                @>
                typesExhausted
