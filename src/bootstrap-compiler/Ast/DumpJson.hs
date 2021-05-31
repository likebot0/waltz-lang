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

withDefault = with @ "ast/dump-json" $ fun \ast -> do
    identifier <- call @ "get-current-src-id" ()

    outDir <- call @ "get-out-dir" ()

    outFilePath <- pure $ outDir ++ identifier

    outAstFilePath <- pure $ outFilePath ++ "/.wz.ast.json"

    sendM $ System.Directory.createDirectoryIfMissing True outFilePath

    sendM $ Bs.writeFile outAstFilePath $ encodePretty $ Aggrigate @ "semantic-analyzed" ast

encode :: ToJSON a => a -> Data.ByteString.Lazy.ByteString
encode = Data.Aeson.encode

encodePretty :: ToJSON a => a -> Data.ByteString.Lazy.ByteString
encodePretty = Data.Aeson.Encode.Pretty.encodePretty' Data.Aeson.Encode.Pretty.Config
    { Data.Aeson.Encode.Pretty.confIndent = Data.Aeson.Encode.Pretty.Tab
    , Data.Aeson.Encode.Pretty.confCompare = Data.Aeson.Encode.Pretty.keyOrder
        [ "pass"
        , "type"
        , "attributes"
        , "severity"
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

instance AttributesToJSON a => ToJSON (Aggrigate a) where
    toJSON (Aggrigate x) = x |>
        do \(x :: Ast.Node a "block-expression") -> toJSON x
        @>
        do \(x :: Ast.Node a "discard") -> toJSON x
        @>
        do \(x :: Ast.Node a "expression") -> toJSON x
        @>
        do \(x :: Ast.Node a "grouped-expression") -> toJSON x
        @>
        do \(x :: Ast.Node a "identifier") -> toJSON x
        @>
        do \(x :: Ast.Node a "key-value") -> toJSON x
        @>
        do \(x :: Ast.Node a "lambda-expression") -> toJSON x
        @>
        do \(x :: Ast.Node a "literal/array") -> toJSON x
        @>
        do \(x :: Ast.Node a "literal/number") -> toJSON x
        @>
        do \(x :: Ast.Node a "literal/object") -> toJSON x
        @>
        do \(x :: Ast.Node a "literal/string") -> toJSON x
        @>
        do \(x :: Ast.Node a "root") -> toJSON x
        @>
        do \(x :: Ast.Node a "root/statement/base") -> toJSON x
        @>
        do \(x :: Ast.Node a "statement/if") -> toJSON x
        @>
        do \(x :: Ast.Node a "statement/let") -> toJSON x
        @>
        do \(x :: Ast.Node a "statement/with") -> toJSON x
        @>
        do \(x :: Ast.Node a "type-expression") -> toJSON x
        @>
        typesExhausted

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

newtype Aggrigate a = Aggrigate (Union (Ast.All a))

newtype SerializableChildren a k = SerializableChildren (Ast.Children a k)

instance (KnownSymbol a, KnownSymbol k, ToJSON (Ast.Attributes a k), ToJSON (SerializableChildren a k)) => ToJSON (Ast.Node a k) where
    toJSON x = object
        [ "type" .= do
            symbolVal do
                Proxy @ k
        , "attributes" .= do
            Ast.attributes x
        , "children" .= do
            SerializableChildren @ a @ k do
                Ast.children x
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "block-expression") where
    toJSON (SerializableChildren body) =
        toJSON do
            Aggrigate @ a . reUnion <$> body

instance AttributesToJSON a => ToJSON (SerializableChildren a "discard") where
    toJSON (SerializableChildren expression) =
        toJSON do
            Aggrigate @ a . liftUnion <$> expression :: Maybe (Aggrigate a)

instance AttributesToJSON a => ToJSON (SerializableChildren a "expression") where
    toJSON (SerializableChildren x) =
        toJSON do
            Aggrigate @ a . reUnion <$> x

instance AttributesToJSON a => ToJSON (SerializableChildren a "grouped-expression") where
    toJSON (SerializableChildren expression) = object
        [ "expression" .= do
            Aggrigate @ a . liftUnion $ expression
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "identifier") where
    toJSON (SerializableChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (SerializableChildren a "key-value") where
    toJSON (SerializableChildren (identifier, expression)) = object
        [ "identifier" .= do
            Aggrigate @ a . liftUnion $ identifier
        , "expression" .= do
            Aggrigate @ a . liftUnion $ expression
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "lambda-expression") where
    toJSON (SerializableChildren (identifier, body)) = object
        [ "identifier" .= identifier
        , "body" .= do
            Aggrigate @ a . reUnion <$> body
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "literal/array") where
    toJSON (SerializableChildren body) =
        toJSON do
            Aggrigate @ a . reUnion <$> body

instance AttributesToJSON a => ToJSON (SerializableChildren a "literal/number") where
    toJSON (SerializableChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (SerializableChildren a "literal/object") where
    toJSON (SerializableChildren body) =
        toJSON do
            Aggrigate @ a . reUnion <$> body

instance AttributesToJSON a => ToJSON (SerializableChildren a "literal/string") where
    toJSON (SerializableChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (SerializableChildren a "type-expression") where
    toJSON (SerializableChildren expression) = object
        [ "expression" .= do
            Aggrigate @ a . liftUnion $ expression
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "root") where
    toJSON (SerializableChildren body) =
        toJSON do
            Aggrigate @ a . reUnion <$> body

instance AttributesToJSON a => ToJSON (SerializableChildren a "root/statement/base") where
    toJSON (SerializableChildren identifier) =
        toJSON do
            Aggrigate @ a . liftUnion $ identifier

instance AttributesToJSON a => ToJSON (SerializableChildren a "statement/if") where
    toJSON (SerializableChildren (predicate, body)) = object
        [ "predicate" .= do
            Aggrigate @ a . liftUnion $ predicate :: Aggrigate a
        , "body" .= do
            Aggrigate @ a . reUnion <$> body
        ]

instance AttributesToJSON a => ToJSON (SerializableChildren a "statement/let") where
    toJSON (SerializableChildren x) =
        toJSON do
            Aggrigate @ a $ liftUnion x

instance AttributesToJSON a => ToJSON (SerializableChildren a "statement/with") where
    toJSON (SerializableChildren body) =
        toJSON do
            Aggrigate @ a . reUnion <$> body
