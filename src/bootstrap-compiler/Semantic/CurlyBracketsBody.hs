module Semantic.CurlyBracketsBody where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.HashMap.Strict
import qualified Semantic.Analyzer
import qualified Semantic.Discard
import qualified Semantic.Shared
import qualified Semantic.KeyValue
import qualified Semantic.Statement.IfStatement
import qualified Semantic.Statement.LetStatement
import qualified Semantic.Statement.WithStatement

type MemberStore = Data.HashMap.Strict.HashMap String (Ast.Node "semantic-analyzed" "expression")

analyze :: Semantic.Analyzer.Constraint e => Ast.CurlyBracketsBody "syntax-analyzed" -> Eff e (Ast.CurlyBracketsBody "semantic-analyzed", MemberStore)
analyze body = do
    Semantic.Shared.newScope \memberStoreRef -> do
        (,)
            <$> sequence do
                body >>=
                    do \(x :: Ast.Node "syntax-analyzed" "discard") -> do
                        pure $ inject <$> Semantic.Discard.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "key-value") -> do
                        pure $ inject <$> do
                            node <- Semantic.KeyValue.analyze x

                            let (identifierNode, expressionNode) = Ast.children node

                            let identifier = Ast.children identifierNode

                            memberStore <- get memberStoreRef

                            set memberStoreRef $ Data.HashMap.Strict.insert identifier expressionNode memberStore

                            pure node
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/if") -> do
                        pure $ inject <$> Semantic.Statement.IfStatement.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/let") -> do
                        pure $ inject <$> do
                            node <- Semantic.Statement.LetStatement.analyze x

                            let (identifierNode, expressionNode) = Ast.children $ Ast.children node

                            let identifier = Ast.children identifierNode

                            memberStore <- get memberStoreRef

                            set memberStoreRef $ Data.HashMap.Strict.insert identifier expressionNode memberStore

                            pure node
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/with") -> do
                        pure $ inject <$> Semantic.Statement.WithStatement.analyze x
                    @>
                    typesExhausted
            <*> get memberStoreRef
