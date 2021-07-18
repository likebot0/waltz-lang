module Waltz.Semantic.Discard where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Expression

analyze :: Waltz.Semantic.Analyzer.Analyze "discard"
analyze x = do
    Waltz.Ast.Node
        <$> do Waltz.Semantic.Expression.analyze $ Waltz.Ast.children x
        <*> do pure $ Waltz.Ast.attributes x
