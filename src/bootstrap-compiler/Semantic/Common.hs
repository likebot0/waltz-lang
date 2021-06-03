module Semantic.Common where

import Global
import Context.Funs
import qualified Ast
import qualified Ast.Semantic
import qualified Data.HashMap.Strict

newScope f = do
    variableStoreRef <- ref do
        Data.HashMap.Strict.empty @ String @ (Ast.Node "semantic-analyzed" "expression")

    do
        f variableStoreRef

        -- Handle local identifiers
        |> with @ "resolve" do
            Fun super <- useContext @ "resolve"

            fun \identifier -> do
                variableStore <- get variableStoreRef

                Data.HashMap.Strict.lookup identifier variableStore |> \case
                    Just x -> return undefined
                    _ -> pure ()

                super identifier
