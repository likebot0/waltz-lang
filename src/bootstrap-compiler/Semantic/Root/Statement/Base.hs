module Semantic.Root.Statement.Base where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.Identifier

analyze :: Semantic.Analyzer.Analyze "root/statement/base"
analyze x = do
    let identifier = Ast.children x

    Ast.Node
        do Ast.attributes x
        <$> Semantic.Identifier.analyze identifier
