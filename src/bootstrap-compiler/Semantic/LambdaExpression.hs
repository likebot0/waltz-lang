module Semantic.LambdaExpression where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.HashMap.Strict
import qualified Semantic.Analyzer
import {-# SOURCE #-} qualified Semantic.CurlyBracketsBody
import qualified Semantic.Identifier

analyze :: Semantic.Analyzer.Analyze "lambda-expression"
analyze x = do
    let (identifier, body) = Ast.children x

    argumentStoreRef <- ref do
        Data.HashMap.Strict.empty @ String @ (Ast.Node "semantic-analyzed" "identifier")

    do
        Ast.Node
            <$> ((,)
                <$> case identifier of
                    Nothing -> pure Nothing
                    Just x -> do
                        result <- Semantic.Identifier.analyze x

                        let Ast.Node identifier _ = result

                        argumentStore <- get argumentStoreRef

                        set argumentStoreRef do
                            Data.HashMap.Strict.insert identifier result argumentStore

                        pure $ Just result
                <*> Semantic.CurlyBracketsBody.analyze body
            )
            <*> do pure $ Ast.attributes x
        -- Handle local identifiers
        |> with @ "resolve" do
            Fun super <- useContext @ "resolve"

            initialSrcId <- call @ "get-current-src-id" ()

            fun \identifier -> do
                currentSrcId <- call @ "get-current-src-id" ()

                if currentSrcId /= initialSrcId
                    then return =<< super identifier
                    else pure ()

                if identifier == "return"
                    then return undefined
                    else pure ()

                argumentStore <- get argumentStoreRef

                Data.HashMap.Strict.lookup identifier argumentStore |> \case
                    Just x -> return undefined
                    _ -> pure ()

                super identifier
