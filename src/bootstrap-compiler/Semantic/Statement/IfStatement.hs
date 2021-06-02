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
        do Ast.attributes x
        <$> ((,)
            <$> do Semantic.Expression.analyze predicate
            <*> do fst <$> Semantic.CurlyBracketsBody.analyze body
        )
