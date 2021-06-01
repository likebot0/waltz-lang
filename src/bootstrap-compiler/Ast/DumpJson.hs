{-# LANGUAGE OverloadedStrings #-}

module Ast.DumpJson where

import Global
import Context.Funs
import Data.Aeson
import Data.Typeable
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as Bs
import qualified Diagnostic
import qualified System.Directory
import qualified TypeFun.Data.List

withDefault = with @ "ast/dump-json" $ fun \ast -> do
    identifier <- call @ "get-current-src-id" ()

    outDir <- call @ "get-out-dir" ()

    outFilePath <- pure $ outDir ++ identifier

    outAstFilePath <- pure $ outFilePath ++ "/.wz.ast.json"

    sendM $ System.Directory.createDirectoryIfMissing True outFilePath

    sendM $ Bs.writeFile outAstFilePath $ encodePretty ast

encode :: ToJSON a => a -> Data.ByteString.Lazy.ByteString
encode = Data.Aeson.encode

encodePretty :: ToJSON a => a -> Data.ByteString.Lazy.ByteString
encodePretty = Data.Aeson.Encode.Pretty.encodePretty' Data.Aeson.Encode.Pretty.Config
    { Data.Aeson.Encode.Pretty.confIndent = Data.Aeson.Encode.Pretty.Tab
    , Data.Aeson.Encode.Pretty.confCompare = Data.Aeson.Encode.Pretty.keyOrder
        [ "type"
        , "attributes"
        , "location"
        , "start"
        , "end"
        , "line-number"
        , "column"
        , "children"
        , "condition"
        , "parameter"
        , "body"
        , "left"
        , "right"
        , "expression"
        ]
    , Data.Aeson.Encode.Pretty.confNumFormat = Data.Aeson.Encode.Pretty.Generic
    , Data.Aeson.Encode.Pretty.confTrailingNewline = False
    }

instance ToJSON (Union '[]) where
    toJSON x = Null

instance (Typeable a, ToJSON a, ToJSON (Union (TypeFun.Data.List.Delete a b))) => ToJSON (Union (a : b)) where
    toJSON x = case restrict @ a x of
        Right x -> toJSON x
        Left x -> toJSON x

newtype Children' a k = Children' (Ast.Children a k)

type AttributesToJSON a = 
    ( Typeable a
    , KnownSymbol a
    , ToJSON (Ast.Attributes a "block-expression")
    , ToJSON (Ast.Attributes a "discard") 
    , ToJSON (Ast.Attributes a "key-value")
    , ToJSON (Ast.Attributes a "expression")
    , ToJSON (Ast.Attributes a "grouped-expression")
    , ToJSON (Ast.Attributes a "identifier")
    , ToJSON (Ast.Attributes a "lambda-expression")
    , ToJSON (Ast.Attributes a "literal/array")
    , ToJSON (Ast.Attributes a "literal/number") 
    , ToJSON (Ast.Attributes a "literal/object") 
    , ToJSON (Ast.Attributes a "literal/string") 
    , ToJSON (Ast.Attributes a "root")
    , ToJSON (Ast.Attributes a "root/statement/base")
    , ToJSON (Ast.Attributes a "type-expression")
    , ToJSON (Ast.Attributes a "statement/if")
    , ToJSON (Ast.Attributes a "statement/let")
    , ToJSON (Ast.Attributes a "statement/with")
    )

instance (KnownSymbol a, KnownSymbol k, ToJSON (Ast.Attributes a k), ToJSON (Children' a k)) => ToJSON (Ast.Node a k) where
    toJSON x =
        object
            [ "type" .= symbolVal do Proxy @ k
            , "attributes" .= Ast.attributes x
            , "children" .= Children' @ a @ k do Ast.children x
            ]

instance AttributesToJSON a => ToJSON (Children' a "block-expression") where
    toJSON (Children' body) =
        toJSON body

instance AttributesToJSON a => ToJSON (Children' a "discard") where
    toJSON (Children' expression) =
        toJSON expression

instance AttributesToJSON a => ToJSON (Children' a "expression") where
    toJSON (Children' terms) =
        toJSON terms

instance AttributesToJSON a => ToJSON (Children' a "grouped-expression") where
    toJSON (Children' expression) =
        toJSON expression

instance AttributesToJSON a => ToJSON (Children' a "identifier") where
    toJSON (Children' x) =
        toJSON x

instance AttributesToJSON a => ToJSON (Children' a "key-value") where
    toJSON (Children' (left, right)) =
        object
            [ "left" .= left
            , "right" .= right
            ]

instance AttributesToJSON a => ToJSON (Children' a "lambda-expression") where
    toJSON (Children' (parameter, body)) =
        object
            [ "parameter" .= parameter
            , "body" .= body
            ]

instance AttributesToJSON a => ToJSON (Children' a "literal/array") where
    toJSON (Children' body) =
        toJSON body

instance AttributesToJSON a => ToJSON (Children' a "literal/number") where
    toJSON (Children' x) =
        toJSON x

instance AttributesToJSON a => ToJSON (Children' a "literal/object") where
    toJSON (Children' body) =
        toJSON body

instance AttributesToJSON a => ToJSON (Children' a "literal/string") where
    toJSON (Children' x) =
        toJSON x

instance AttributesToJSON a => ToJSON (Children' a "type-expression") where
    toJSON (Children' expression) =
        object
            [ "expression" .= expression
            ]

instance AttributesToJSON a => ToJSON (Children' a "root") where
    toJSON (Children' body) =
        toJSON body

instance AttributesToJSON a => ToJSON (Children' a "root/statement/base") where
    toJSON (Children' identifier) =
        toJSON identifier

instance AttributesToJSON a => ToJSON (Children' a "statement/if") where
    toJSON (Children' (condition, body)) =
        object
            [ "condition" .= condition
            , "body" .= body
            ]

instance AttributesToJSON a => ToJSON (Children' a "statement/let") where
    toJSON (Children' x) =
        toJSON x

instance AttributesToJSON a => ToJSON (Children' a "statement/with") where
    toJSON (Children' body) =
        toJSON body
