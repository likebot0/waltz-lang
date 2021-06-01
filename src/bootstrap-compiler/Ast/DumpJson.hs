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
        , "predicate"
        , "parameter"
        , "body"
        , "identifier"
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

newtype ChildrenContainer a k = ChildrenContainer (Ast.Children a k)

instance (KnownSymbol a, KnownSymbol k, ToJSON (Ast.Attributes a k), ToJSON (ChildrenContainer a k)) => ToJSON (Ast.Node a k) where
    toJSON x = object
        [ "type" .= symbolVal do Proxy @ k
        , "attributes" .= Ast.attributes x
        , "children" .= ChildrenContainer @ a @ k do Ast.children x
        ]

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

instance AttributesToJSON a => ToJSON (ChildrenContainer a "block-expression") where
    toJSON (ChildrenContainer body) =
        toJSON body

instance AttributesToJSON a => ToJSON (ChildrenContainer a "discard") where
    toJSON (ChildrenContainer expression) =
        toJSON expression

instance AttributesToJSON a => ToJSON (ChildrenContainer a "expression") where
    toJSON (ChildrenContainer terms) =
        toJSON terms

instance AttributesToJSON a => ToJSON (ChildrenContainer a "grouped-expression") where
    toJSON (ChildrenContainer expression) =
        toJSON expression

instance AttributesToJSON a => ToJSON (ChildrenContainer a "identifier") where
    toJSON (ChildrenContainer x) =
        toJSON x

instance AttributesToJSON a => ToJSON (ChildrenContainer a "key-value") where
    toJSON (ChildrenContainer (identifier, expression)) = object
        [ "identifier" .= identifier
        , "expression" .= expression
        ]

instance AttributesToJSON a => ToJSON (ChildrenContainer a "lambda-expression") where
    toJSON (ChildrenContainer (parameter, body)) = object
        [ "parameter" .= parameter
        , "body" .= body
        ]

instance AttributesToJSON a => ToJSON (ChildrenContainer a "literal/array") where
    toJSON (ChildrenContainer body) =
        toJSON body

instance AttributesToJSON a => ToJSON (ChildrenContainer a "literal/number") where
    toJSON (ChildrenContainer x) =
        toJSON x

instance AttributesToJSON a => ToJSON (ChildrenContainer a "literal/object") where
    toJSON (ChildrenContainer body) =
        toJSON body

instance AttributesToJSON a => ToJSON (ChildrenContainer a "literal/string") where
    toJSON (ChildrenContainer x) = toJSON x

instance AttributesToJSON a => ToJSON (ChildrenContainer a "type-expression") where
    toJSON (ChildrenContainer expression) = object
        [ "expression" .= expression
        ]

instance AttributesToJSON a => ToJSON (ChildrenContainer a "root") where
    toJSON (ChildrenContainer body) =
        toJSON body

instance AttributesToJSON a => ToJSON (ChildrenContainer a "root/statement/base") where
    toJSON (ChildrenContainer identifier) =
        toJSON identifier

instance AttributesToJSON a => ToJSON (ChildrenContainer a "statement/if") where
    toJSON (ChildrenContainer (predicate, body)) = object
        [ "predicate" .= predicate
        , "body" .= body
        ]

instance AttributesToJSON a => ToJSON (ChildrenContainer a "statement/let") where
    toJSON (ChildrenContainer x) =
        toJSON x

instance AttributesToJSON a => ToJSON (ChildrenContainer a "statement/with") where
    toJSON (ChildrenContainer body) =
        toJSON body
