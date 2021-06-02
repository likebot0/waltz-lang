module Semantic.Statement.LetStatement where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.HashMap.Strict
import qualified Semantic.Analyzer
import qualified Semantic.KeyValue

type VariableStore = Data.HashMap.Strict.HashMap String (Ast.Node "semantic-analyzed" "expression")

analyze :: IORef VariableStore -> Semantic.Analyzer.Analyze "statement/let"
analyze variableStoreRef x = do
    Ast.Node
        <$> do Semantic.KeyValue.analyze variableStoreRef $ Ast.children x
        <*> do pure $ Ast.attributes x
