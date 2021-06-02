module Semantic.Statement.BaseStatement where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.Identifier

analyze :: Semantic.Analyzer.Analyze "statement/base"
analyze x = do
    let identifier = Ast.children x

    Ast.Node
        <$> Semantic.Identifier.analyze identifier
        <*> do pure $ Ast.attributes x
