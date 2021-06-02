module Semantic.Literal.Object where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.Common
import qualified Semantic.Discard
import qualified Semantic.KeyValue
import qualified Semantic.Statement.IfStatement
import qualified Semantic.Statement.IncludeStatement
import qualified Semantic.Statement.LetStatement
import qualified Semantic.Statement.WithStatement
import qualified Data.HashMap.Strict

analyze :: Semantic.Analyzer.Analyze "literal/object"
analyze x =
    Semantic.Common.newScope \memberStoreRef -> do
        Ast.Node
            <$> mapM
                (
                    do \(x :: Ast.Node "syntax-analyzed" "discard") ->
                        inject <$> Semantic.Discard.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "key-value") ->
                        inject <$> Semantic.KeyValue.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/if") ->
                        inject <$> Semantic.Statement.IfStatement.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/include") ->
                        inject <$> Semantic.Statement.IncludeStatement.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/let") ->
                        inject <$> Semantic.Statement.LetStatement.analyze memberStoreRef x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/with") ->
                        inject <$> Semantic.Statement.WithStatement.analyze x
                    @>
                    typesExhausted
                )
                do Ast.children x
            <*> do
                Ast.Semantic.CurlyBracketsAttributes
                    <$> do pure $ Ast.Syntax.location $ Ast.attributes x
                    <*> do get memberStoreRef
