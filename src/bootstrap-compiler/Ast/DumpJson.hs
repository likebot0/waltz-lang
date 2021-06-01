{-# LANGUAGE OverloadedStrings #-}

module Ast.DumpJson where

import Global
import Context.Funs
import Data.Aeson
import Data.Typeable ( Typeable )
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as Bs
import qualified Data.Text
import qualified Diagnostic
import qualified System.Directory

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
        , "key"
        , "value"
        , "expression"
        ]
    , Data.Aeson.Encode.Pretty.confNumFormat = Data.Aeson.Encode.Pretty.Generic
    , Data.Aeson.Encode.Pretty.confTrailingNewline = False
    }

newtype BoxedChildren a k = BoxedChildren (Ast.Children a k)

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

instance (KnownSymbol a, KnownSymbol k, ToJSON (Ast.Attributes a k), ToJSON (BoxedChildren a k)) => ToJSON (Ast.Node a k) where
    toJSON x =
        object
            [ "type" .= do
                toJSON $ show $ Proxy @ k
            , "attributes" .= do
                Ast.attributes x
            , "children" .= do
                BoxedChildren @ a @ k $ Ast.children x
            ]

instance AttributesToJSON a => ToJSON (BoxedChildren a "block-expression") where
    toJSON (BoxedChildren body) =
        toJSON body

instance AttributesToJSON a => ToJSON (BoxedChildren a "discard") where
    toJSON (BoxedChildren expression) =
        toJSON expression

instance AttributesToJSON a => ToJSON (BoxedChildren a "expression") where
    toJSON (BoxedChildren terms) =
        toJSON terms

instance AttributesToJSON a => ToJSON (BoxedChildren a "grouped-expression") where
    toJSON (BoxedChildren expression) =
        toJSON expression

instance AttributesToJSON a => ToJSON (BoxedChildren a "identifier") where
    toJSON (BoxedChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (BoxedChildren a "key-value") where
    toJSON (BoxedChildren (key, value)) =
        object
            [ "key" .= key
            , "value" .= value
            ]

instance AttributesToJSON a => ToJSON (BoxedChildren a "lambda-expression") where
    toJSON (BoxedChildren (parameter, body)) =
        object
            [ "parameter" .= parameter
            , "body" .= body
            ]

instance AttributesToJSON a => ToJSON (BoxedChildren a "literal/array") where
    toJSON (BoxedChildren body) =
        toJSON body

instance AttributesToJSON a => ToJSON (BoxedChildren a "literal/number") where
    toJSON (BoxedChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (BoxedChildren a "literal/object") where
    toJSON (BoxedChildren body) =
        toJSON body

instance AttributesToJSON a => ToJSON (BoxedChildren a "literal/string") where
    toJSON (BoxedChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (BoxedChildren a "type-expression") where
    toJSON (BoxedChildren expression) =
        object
            [ "expression" .= expression
            ]

instance AttributesToJSON a => ToJSON (BoxedChildren a "root") where
    toJSON (BoxedChildren body) =
        toJSON body

instance AttributesToJSON a => ToJSON (BoxedChildren a "root/statement/base") where
    toJSON (BoxedChildren identifier) =
        toJSON identifier

instance AttributesToJSON a => ToJSON (BoxedChildren a "statement/if") where
    toJSON (BoxedChildren (condition, body)) =
        object
            [ "condition" .= condition
            , "body" .= body
            ]

instance AttributesToJSON a => ToJSON (BoxedChildren a "statement/let") where
    toJSON (BoxedChildren x) =
        toJSON x

instance AttributesToJSON a => ToJSON (BoxedChildren a "statement/with") where
    toJSON (BoxedChildren body) =
        toJSON body
