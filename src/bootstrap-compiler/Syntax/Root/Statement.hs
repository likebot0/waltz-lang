module Syntax.Root.Statement where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Shared
import qualified Syntax.Root.Statement.Base

analyzer :: Syntax.Analyzer.Analyzer (Union (Ast.RootStatement "syntax-analyzed"))
analyzer = choice
    [ inject <$> Syntax.Root.Statement.Base.analyzer
    ]
