module Context.Funs where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Diagnostic

type Implemented =
    [ UseContext "ast/dump"
    , UseContext "ast/dump-json"
    , UseContext "fs/read-file"
    , UseContext "get-current-src-id"
    , UseContext "get-entrypoints"
    , UseContext "get-path-by-id"
    , UseContext "get-opt-dirs"
    , UseContext "get-out-dir"
    , UseContext "get-src-dirs"
    , UseContext "resolve"
    ]

instance DeclareContextFun "ast/dump" where
    type instance Input "ast/dump" = Union (Ast.All "semantic-analyzed")

    type instance Require "ast/dump" =
        '[
        ]
            :++: Implemented
            :++: Require "ast/dump-json"

instance DeclareContextFun "ast/dump-json" where
    type instance Input "ast/dump-json" = Union (Ast.All "semantic-analyzed")

    type instance Require "ast/dump-json" =
        Implemented

instance DeclareContextFun "get-current-src-id" where
    type instance Output "get-current-src-id" = String

instance DeclareContextFun "get-current-src-path" where
    type instance Output "get-current-src-path" = String

instance DeclareContextFun "diagnostic/send" where
    type instance Input "diagnostic/send" = Diagnostic.Diagnostic

    type instance Require "diagnostic/send" =
        '[ UseContext "get-current-src-path"
        ]
            :++: Implemented

instance DeclareContextFun "get-entrypoints" where
    type instance Output "get-entrypoints" = [String]

instance DeclareContextFun "get-out-dir" where
    type instance Output "get-out-dir" = String

instance DeclareContextFun "get-opt-dirs" where
    type instance Output "get-opt-dirs" = [String]

    type instance Require "get-opt-dirs" = Implemented

instance DeclareContextFun "get-path-by-id" where
    type instance Input "get-path-by-id" = String

    type instance Output "get-path-by-id" = String

    type instance Require "get-path-by-id" = Implemented

instance DeclareContextFun "get-src-dirs" where
    type instance Output "get-src-dirs" = [String]

instance DeclareContextFun "fs/read-file" where
    type instance Input "fs/read-file" = String

    type instance Output "fs/read-file" = String

    type instance Require "fs/read-file" = Implemented

instance DeclareContextFun "resolve" where
    type instance Input "resolve" = String

    type instance Output "resolve" = Union
        [ Ast.Node "semantic-analyzed" "expression"
        , Ast.Node "semantic-analyzed" "root"
        ]

    type instance Require "resolve" =
        '[ UseContext "diagnostic/send"
        ]
            :++: Implemented
