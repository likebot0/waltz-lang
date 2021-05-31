module Semantic.BlockExpression where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import qualified Semantic.CurlyBracketsBody

analyze :: Semantic.Analyzer.Analyze "block-expression"
analyze x = do
    let body = Ast.children x

    Ast.Node
        do Ast.attributes x
        <$> do
            fst <$> Semantic.CurlyBracketsBody.analyze body

    |> with @ "resolve" do
        Fun super <- useContext @ "resolve"

        initialSrcId <- call @ "get-current-src-id" ()

        fun \identifier -> do
            currentSrcId <- call @ "get-current-src-id" ()

            if currentSrcId /= initialSrcId
                then return =<< super identifier
                else pure ()

            if identifier == "break"
                then return undefined
                else pure ()

            super identifier
