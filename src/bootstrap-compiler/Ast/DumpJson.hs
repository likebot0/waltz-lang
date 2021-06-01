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
        [ "pass"
        , "type"
        , "attributes"
        , "location"
        , "start"
        , "end"
        , "line-number"
        , "column"
        , "data"
        , "predicate-previous-comments"
        , "predicate"
        , "head"
        , "body-previous-comments"
        , "body"
        , "body-next-comments"
        , "identifier"
        , "identifier-next-comments"
        , "expression-previous-comments"
        , "expression"
        ]
    , Data.Aeson.Encode.Pretty.confNumFormat = Data.Aeson.Encode.Pretty.Generic
    , Data.Aeson.Encode.Pretty.confTrailingNewline = False
    }

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

newtype SerializableChildren a k = SerializableChildren (Ast.Children a k)

instance ToJSON (Union '[]) where
    toJSON x = Null

instance (Typeable a, ToJSON a, ToJSON (Union (TypeFun.Data.List.Delete a b))) => ToJSON (Union (a : b)) where
    toJSON x = case restrict @ a x of
        Right x -> toJSON x
        Left x -> toJSON x

instance (KnownSymbol a, KnownSymbol k, ToJSON (Ast.Attributes a k), ToJSON (SerializableChildren a k)) => ToJSON (Ast.Node a k) where
    toJSON x = object
        [ "type" .= symbolVal do Proxy @ k
        , "attributes" .= Ast.attributes x
        , "children" .= SerializableChildren @ a @ k do Ast.children x
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "block-expression") where
    toJSON (SerializableChildren body) =
        toJSON body

instance AttributesToJSON a => ToJSON (SerializableChildren a "discard") where
    toJSON (SerializableChildren expression) = object
        [ "expression" .= expression
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "expression") where
    toJSON (SerializableChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (SerializableChildren a "grouped-expression") where
    toJSON (SerializableChildren expression) = object
        [ "expression" .= expression
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "identifier") where
    toJSON (SerializableChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (SerializableChildren a "key-value") where
    toJSON (SerializableChildren (identifier, expression)) = object
        [ "identifier" .= identifier
        , "expression" .= expression
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "lambda-expression") where
    toJSON (SerializableChildren (identifier, body)) = object
        [ "identifier" .= identifier
        , "body" .= body
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "literal/array") where
    toJSON (SerializableChildren body) =
        toJSON body

instance AttributesToJSON a => ToJSON (SerializableChildren a "literal/number") where
    toJSON (SerializableChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (SerializableChildren a "literal/object") where
    toJSON (SerializableChildren body) =
        toJSON body

instance AttributesToJSON a => ToJSON (SerializableChildren a "literal/string") where
    toJSON (SerializableChildren x) = ""

instance AttributesToJSON a => ToJSON (SerializableChildren a "type-expression") where
    toJSON (SerializableChildren expression) = object
        [ "expression" .= expression
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "root") where
    toJSON (SerializableChildren body) =
        toJSON body

instance AttributesToJSON a => ToJSON (SerializableChildren a "root/statement/base") where
    toJSON (SerializableChildren identifier) =
        toJSON identifier

instance AttributesToJSON a => ToJSON (SerializableChildren a "statement/if") where
    toJSON (SerializableChildren (predicate, body)) = object
        [ "predicate" .= predicate
        , "body" .= body
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "statement/let") where
    toJSON (SerializableChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (SerializableChildren a "statement/with") where
    toJSON (SerializableChildren body) =
        toJSON body
