module Semantic.Statement.IfStatement where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import {-# SOURCE #-} qualified Semantic.CurlyBracketsBody
import {-# SOURCE #-} qualified Semantic.Expression

analyze :: Semantic.Analyzer.Analyze "statement/if"
analyze x = do
    let (predicate, body) = Ast.children x

    Ast.Node
        <$> ((,)
            <$> Semantic.Expression.analyze predicate
            <*> Semantic.CurlyBracketsBody.analyze body
        )
        <*> do pure $ Ast.attributes x
