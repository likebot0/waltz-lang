{-# LANGUAGE OverloadedStrings #-}

module Ast.Semantic where

import Global
import Data.Aeson
import qualified Ast
import qualified Data.HashMap.Strict
import qualified Diagnostic

type instance Ast.Attributes "semantic-analyzed" "block-expression" =
    Ast.Attributes "syntax-analyzed" "block-expression"

type instance Ast.Attributes "semantic-analyzed" "discard" =
    Ast.Attributes "syntax-analyzed" "discard"

type instance Ast.Attributes "semantic-analyzed" "expression" =
    ExpressionAttributes

type instance Ast.Attributes "semantic-analyzed" "grouped-expression" =
    Ast.Attributes "syntax-analyzed" "grouped-expression"

type instance Ast.Attributes "semantic-analyzed" "identifier" =
    Ast.Attributes "syntax-analyzed" "identifier"

type instance Ast.Attributes "semantic-analyzed" "key-value" =
    Ast.Attributes "syntax-analyzed" "key-value"

type instance Ast.Attributes "semantic-analyzed" "lambda-expression" =
    Ast.Attributes "syntax-analyzed" "lambda-expression"

type instance Ast.Attributes "semantic-analyzed" "literal/array" =
    Ast.Attributes "syntax-analyzed" "literal/array"

type instance Ast.Attributes "semantic-analyzed" "literal/number" =
    Ast.Attributes "syntax-analyzed" "literal/number"

type instance Ast.Attributes "semantic-analyzed" "literal/object" =
    CurlyBracketsAttributes

type instance Ast.Attributes "semantic-analyzed" "literal/string" =
    Ast.Attributes "syntax-analyzed" "literal/string"

type instance Ast.Attributes "semantic-analyzed" "root" =
    Ast.Attributes "syntax-analyzed" "root"

type instance Ast.Attributes "semantic-analyzed" "type-expression" =
    Ast.Attributes "syntax-analyzed" "type-expression"

type instance Ast.Attributes "semantic-analyzed" "statement/base" =
    Ast.Attributes "syntax-analyzed" "statement/base"

type instance Ast.Attributes "semantic-analyzed" "statement/include" =
    Ast.Attributes "syntax-analyzed" "statement/include"

type instance Ast.Attributes "semantic-analyzed" "statement/if" =
    Ast.Attributes "syntax-analyzed" "statement/if"

type instance Ast.Attributes "semantic-analyzed" "statement/let" =
    Ast.Attributes "syntax-analyzed" "statement/let"

type instance Ast.Attributes "semantic-analyzed" "statement/with" =
    Ast.Attributes "syntax-analyzed" "statement/with"

newtype Type s = Type (Data s)

type All = Union
    [ Type "array"
    , Type "function"
    , Type "number"
    , Type "object"
    , Type "string"
    , Type "unknown"
    ]

type family Data (s :: Symbol)

type instance Data "array" =
    [All]

type instance Data "function" =
    String

type instance Data "number" =
    String

type instance Data "object" =
    Data.HashMap.Strict.HashMap String All

type instance Data "string" = 
    String

type instance Data "unknown" =
    ()

data CurlyBracketsAttributes =
    CurlyBracketsAttributes
        Diagnostic.Location
        (Data.HashMap.Strict.HashMap String (Ast.Node "semantic-analyzed" "expression"))

data ExpressionAttributes =
    ExpressionAttributes
        Diagnostic.Location
        All

instance Data.Aeson.ToJSON CurlyBracketsAttributes where
    toJSON (CurlyBracketsAttributes location _) = object
        [ "location" .= location
        ]

instance Data.Aeson.ToJSON ExpressionAttributes where
    toJSON (ExpressionAttributes location _) = object
        [ "location" .= location
        ]
