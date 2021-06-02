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
    let (key, value) = Ast.children x

    key <- Semantic.Identifier.analyze key

    value <- Semantic.Expression.analyze value

    variableStore <- get variableStoreRef

    set variableStoreRef $ Data.HashMap.Strict.insert (Ast.children key) value variableStore

    pure $ Ast.Node (key, value) $ Ast.attributes x
