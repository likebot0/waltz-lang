module Semantic.TypeExpression where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import {-# SOURCE #-} qualified Semantic.Expression

analyze :: Semantic.Analyzer.Analyze "type-expression"
analyze x = do
    let expression = Ast.children x

    Ast.Node
        do Ast.attributes x
        <$> Semantic.Expression.analyze expression
