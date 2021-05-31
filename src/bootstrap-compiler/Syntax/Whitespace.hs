module Syntax.Whitespace where

import Global
import Text.Megaparsec
import qualified Syntax.Analyzer

analyzer :: Syntax.Analyzer.Analyzer ()
analyzer = () <$ oneOf " \t\r\n"
