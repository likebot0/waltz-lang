module Semantic.Literal.Array where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.Discard
import {-# SOURCE #-} qualified Semantic.Expression
import qualified Semantic.Statement.IfStatement
import qualified Semantic.Statement.LetStatement
import qualified Semantic.Statement.WithStatement
import qualified Semantic.Shared
import qualified Data.HashMap.Strict

analyze :: Semantic.Analyzer.Analyze "literal/array"
analyze x = do
    Semantic.Shared.newScope \memberStoreRef -> do
        Ast.Node
            do Ast.attributes x
            <$> sequence do
                Ast.children x >>=
                    do \(x :: Ast.Node "syntax-analyzed" "discard") -> do
                        pure $ liftUnion <$> Semantic.Discard.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "expression") -> do
                        pure $ liftUnion <$> Semantic.Expression.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/if") -> do
                        pure $ liftUnion <$> Semantic.Statement.IfStatement.analyze x
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/let") -> do
                        pure $ liftUnion <$> do
                            node <- Semantic.Statement.LetStatement.analyze x

                            let (identifierNode, expressionNode) = Ast.children $ Ast.children node

                            let identifier = Ast.children identifierNode

                            memberStore <- get memberStoreRef

                            set memberStoreRef $ Data.HashMap.Strict.insert identifier expressionNode memberStore

                            pure node
                    @>
                    do \(x :: Ast.Node "syntax-analyzed" "statement/with") -> do
                        pure $ liftUnion <$> Semantic.Statement.WithStatement.analyze x
                    @>
                    typesExhausted
