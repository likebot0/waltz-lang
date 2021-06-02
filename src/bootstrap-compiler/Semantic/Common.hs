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

            initialSrcId <- call @ "get-current-src-id" ()

            fun \identifier -> do
                currentSrcId <- call @ "get-current-src-id" ()

                if currentSrcId /= initialSrcId
                    then return =<< super identifier
                    else pure ()

                variableStore <- get variableStoreRef

                Data.HashMap.Strict.lookup identifier variableStore |> \case
                    Just x -> return undefined
                    _ -> pure ()

                super identifier
