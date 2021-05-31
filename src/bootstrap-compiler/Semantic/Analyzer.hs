module Semantic.Analyzer where

import Global
import Context.Funs
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Text.Megaparsec

type Analyze s = forall e. Constraint e => Ast.Node "syntax-analyzed" s -> Eff e (Ast.Node "semantic-analyzed" s)

type Constraint e =
    ( Member (UseContext "get-current-src-id") e
    , Member (UseContext "get-current-src-path") e
    , Members (Require "resolve") e
    , Member Raise e
    , LastMember IO e
    )
