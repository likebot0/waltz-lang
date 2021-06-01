module Action.Resolve where

import Global
import Context.Funs
import qualified Ast
import qualified Ast.Semantic
import qualified Data.HashMap.Strict
import qualified Diagnostic
import qualified Error.CannotResolve
import qualified Error.BreakNotInBlock
import qualified Error.ReturnNotInFunction
import qualified Fs.ReadFile
import qualified Semantic.Analyzer
import qualified Semantic.Root
import qualified Syntax.Root
import qualified Text.Megaparsec

withDefault x = x
    -- Handle reserved identifiers
    |> with @ "resolve" do
        Fun super <- useContext @ "resolve"

        fun \identifier -> do
            if Data.HashMap.Strict.member identifier reservedIdentifiers
                then return undefined
                else pure ()

            super identifier

    -- Reject local identifiers
    |> with @ "resolve" do
        Fun super <- useContext @ "resolve"

        fun \identifier -> do
            identifier |> \case
                '/' : _ ->
                    pure ()
                "break" ->
                    raise Error.BreakNotInBlock.Type
                "return" ->
                    raise Error.ReturnNotInFunction.Type
                _ ->
                    raise $ Error.CannotResolve.Type identifier

            super identifier

    -- Caching
    |> with @ "resolve" do
        Fun super <- useContext @ "resolve"

        cacheStoreRef <- ref do
            Data.HashMap.Strict.empty @ (Input "resolve") @ (Output "resolve")

        fun \identifier -> do
            cacheStore <- get cacheStoreRef

            Data.HashMap.Strict.lookup identifier cacheStore |> \case
                Just x -> return x
                _ -> pure ()

            result <- super identifier

            set cacheStoreRef $ Data.HashMap.Strict.insert identifier result cacheStore

            return result

    -- Verbose output
    |> with @ "resolve" do
        Fun super <- useContext @ "resolve"

        fun \identifier -> do
            sendM $ putStrLn $ "Loading " ++ identifier

            super identifier

    -- AST dump
    |> with @ "resolve" do
        Fun super <- useContext @ "resolve"

        fun \identifier -> do
            ast <- super identifier

            call @ "ast/dump" (reinterpret ast)
                |> with @ "get-current-src-id" do
                    fun \_ -> do
                        return identifier

            return ast

    |> with @ "resolve" do
        fun \identifier -> do
            path <- call @ "get-path-by-id" identifier

            source <- call @ "fs/read-file" path

            syntaxAst <- do
                parsed <- Text.Megaparsec.runParserT'
                    do Syntax.Root.analyzer
                    do initialState path 1 source
                
                parsed |> \case
                    (_, Left x) -> raise $ "Internal compiler error: "  ++ show x
                    (_, Right x) -> pure x

                |> with @ "get-current-src-id" do
                    fun \_ -> do
                        return identifier

                |> with @ "get-current-src-path" do
                    fun \_ -> do
                        return path

            semanticAst <- do
                Semantic.Root.analyze syntaxAst

                |> with @ "get-current-src-id" do
                    fun \_ -> do
                        return identifier

                |> with @ "get-current-src-path" do
                    fun \_ -> do
                        return path

            pure $ inject semanticAst

            |> with @ "get-current-src-id" do
                fun \_ -> do
                    return identifier

reservedIdentifiers :: Data.HashMap.Strict.HashMap String String
reservedIdentifiers = Data.HashMap.Strict.fromList
    [ (,) "infer"
        "/std/infer"
    , (,) "number"
        "/std/number"
    , (,) "string"
        "/std/string"
    , (,) "boolean"
        "/std/boolean"
    , (,) "true"
        "/std/boolean/true"
    , (,) "false"
        "/std/boolean/false"
    , (,) "not"
        "/std/boolean/not"
    , (,) "null"
        "/std/null"
    , (,) "size"
        "/std/size"
    , (,) "u8"
        "/std/u8"
    , (,) "u16"
        "/std/u16"
    , (,) "u32"
        "/std/u32"
    , (,) "u64"
        "/std/u64"
    , (,) "i8"
        "/std/i8"
    , (,) "i16"
        "/std/i16"
    , (,) "i32"
        "/std/i32"
    , (,) "i64"
        "/std/i64"
    , (,) "spawn"
        "/std/spawn"
    , (,) "await"
        "/std/await"
    , (,) "$"
        ""
    , (,) "="
        ""
    , (,) "/="
        ""
    , (,) "+"
        ""
    , (,) "-"
        ""
    , (,) "*"
        ""
    , (,) "/"
        ""
    , (,) "|"
        ""
    , (,) "&"
        ""
    , (,) "_"
        ""
    ]

initialState path tabWidth s = Text.Megaparsec.State
    { Text.Megaparsec.stateInput  = s
    , Text.Megaparsec.stateOffset = 0
    , Text.Megaparsec.statePosState = Text.Megaparsec.PosState
        { Text.Megaparsec.pstateInput = s
        , Text.Megaparsec.pstateOffset = 0
        , Text.Megaparsec.pstateSourcePos = Text.Megaparsec.initialPos path
        , Text.Megaparsec.pstateTabWidth = Text.Megaparsec.mkPos tabWidth
        , Text.Megaparsec.pstateLinePrefix = ""
        }
    , Text.Megaparsec.stateParseErrors = []
    }
