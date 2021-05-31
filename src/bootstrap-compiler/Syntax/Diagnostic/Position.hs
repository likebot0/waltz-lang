module Syntax.Diagnostic.Position where

import Global
import Diagnostic
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax

analyzer :: (TraversableStream s, MonadParsec e s m) => m Diagnostic.Position
analyzer = do
    x <- getSourcePos

    pure $ Diagnostic.Position
        do unPos $ sourceLine x
        do unPos $ sourceColumn x
