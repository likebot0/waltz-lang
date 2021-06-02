module Semantic.KeyValue where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.HashMap.Strict
import {-# SOURCE #-} qualified Semantic.Expression
import qualified Semantic.Analyzer
import qualified Semantic.Identifier

type VariableStore = Data.HashMap.Strict.HashMap String (Ast.Node "semantic-analyzed" "expression")

analyze :: IORef VariableStore -> Semantic.Analyzer.Analyze "key-value"
analyze variableStoreRef x = do
    let (identifier, expression) = Ast.children x

    Ast.Node
        <$> ((,)
            <$> Semantic.Identifier.analyze identifier
            <*> Semantic.Expression.analyze expression
        )
        <*> do pure $ Ast.attributes x
