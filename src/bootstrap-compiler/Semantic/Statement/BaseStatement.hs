module Semantic.Statement.BaseStatement where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.Identifier

analyze :: Semantic.Analyzer.Analyze "statement/base"
analyze x = do
    Ast.Node
        <$> do Semantic.Identifier.analyze $ Ast.children x
        <*> do pure $ Ast.attributes x
