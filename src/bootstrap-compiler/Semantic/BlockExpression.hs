module Semantic.BlockExpression where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.CurlyBracketsBody

analyze :: Semantic.Analyzer.Analyze "block-expression"
analyze x = do
    Ast.Node
        <$> do Semantic.CurlyBracketsBody.analyze $ Ast.children x
        <*> do pure $ Ast.attributes x

    |> with @ "resolve" do
        Fun super <- useContext @ "resolve"

        fun \identifier -> do
            if identifier == "break"
                then return undefined
                else pure ()

            super identifier
