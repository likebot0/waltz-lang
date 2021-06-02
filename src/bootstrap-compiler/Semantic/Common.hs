module Semantic.Common where

import Global
import Context.Funs
import qualified Ast
import qualified Ast.Semantic
import qualified Data.HashMap.Strict

newScope f = do
    memberStoreRef <- ref do
        Data.HashMap.Strict.empty @ String @ (Ast.Node "semantic-analyzed" "expression")

    do
        f memberStoreRef

        -- Handle local identifiers
        |> with @ "resolve" do
            Fun super <- useContext @ "resolve"

            initialSrcId <- call @ "get-current-src-id" ()

            fun \identifier -> do
                currentSrcId <- call @ "get-current-src-id" ()

                if currentSrcId /= initialSrcId
                    then return =<< super identifier
                    else pure ()

                memberStore <- get memberStoreRef

                Data.HashMap.Strict.lookup identifier memberStore |> \case
                    Just x -> return undefined
                    _ -> pure ()

                super identifier
