module Action.Resolve.C where

import Global
import Context.Funs
import qualified Ast
import qualified Ast.Semantic
import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8
import qualified Data.List
import qualified Data.List.Split
import qualified Error.CannotResolve
import qualified System.Process

-- withDefault = with @ "resolve/c" $ fun \identifier -> do
--     pure $ Ast.Node () ()

    -- do
    --     case Data.List.Split.splitOn "/" identifier of
    --         [_, _, identifier] -> do
    --             case identifier of
    --                 "pointer" -> 
    --                     pure $ Ast.Node () identifier
    --                 _ -> 
    --                     raise $ Error.CannotResolve.Type identifier

    --         [_, _, includePath, identifier] -> do
    --             let command = "clang"

    --             let arguments = ["-x", "c", "-Xclang", "-ast-dump=json", "-fsyntax-only", "-fno-color-diagnostics", "-"]

    --             output <- sendM $ System.Process.readProcess command arguments $ "#include <" ++ includePath ++ ">"

    --             nodes <- case Data.Aeson.decode (Data.ByteString.Lazy.Char8.pack output) of
    --                 Nothing -> raise $ Error.CannotResolve.Type identifier
    --                 Just (Plugin.Clang.TranslationUnitDecl x) -> pure x

    --             let match = \case
    --                     Plugin.Clang.Node _ (Just name) _ _ -> name == identifier
    --                     _ -> False 

    --             node <- case Data.List.find match nodes of
    --                 Nothing -> raise $ Error.CannotResolve.Type identifier
    --                 Just x -> pure x

    --             case node of
    --                 (Plugin.Clang.Node _ _ (Just (Plugin.Clang.Type (Just t))) _) ->
    --                     pure $ Ast.Node () t

    --         _ -> do
    --             raise $ Error.CannotResolve.Type identifier

    --     |> with @ "get-current-src-path" do
    --         fun \_ -> do
    --             return identifier
