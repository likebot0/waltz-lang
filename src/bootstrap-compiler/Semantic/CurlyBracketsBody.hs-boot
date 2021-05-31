module Semantic.CurlyBracketsBody where

import Global
import qualified Ast
import qualified Data.HashMap.Strict
import qualified Semantic.Analyzer

type MemberStore = Data.HashMap.Strict.HashMap String (Ast.Node "semantic-analyzed" "expression")

analyze :: Semantic.Analyzer.Constraint e => Ast.CurlyBracketsBody "syntax-analyzed" -> Eff e (Ast.CurlyBracketsBody "semantic-analyzed", MemberStore)
