module Syntax.Separator where

import Global
import Text.Megaparsec
import qualified Syntax.Analyzer

analyzer :: Syntax.Analyzer.Analyzer ()
analyzer = () <$ single ','
