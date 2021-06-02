module Semantic.Statement.LetStatement where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.HashMap.Strict
import qualified Semantic.Analyzer
import qualified Semantic.KeyValue

analyze :: IORef (Data.HashMap.Strict.HashMap String (Ast.Node "semantic-analyzed" "expression")) -> Semantic.Analyzer.Analyze "statement/let"
analyze variableStoreRef x = do
    keyValue <- Semantic.KeyValue.analyze $ Ast.children x

    let (key, value) = Ast.children keyValue

    variableStore <- get variableStoreRef

    set variableStoreRef $ Data.HashMap.Strict.insert (Ast.children key) value variableStore

    pure $ Ast.Node
        do keyValue
        do Ast.attributes x
