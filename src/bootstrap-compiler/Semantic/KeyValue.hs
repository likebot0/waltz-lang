module Semantic.KeyValue where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import {-# SOURCE #-} qualified Semantic.Expression
import qualified Semantic.Analyzer
import qualified Semantic.Identifier

analyze :: Semantic.Analyzer.Analyze "key-value"
analyze x = do
    let (identifier, expression) = Ast.children x

    Ast.Node
        do Ast.attributes x
        <$> ((,)
            <$> Semantic.Identifier.analyze identifier
            <*> Semantic.Expression.analyze expression
        )
