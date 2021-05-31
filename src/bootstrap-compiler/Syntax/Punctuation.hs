module Syntax.Punctuation where

import Global
import Text.Megaparsec
import qualified Syntax.Analyzer

analyzer :: Syntax.Analyzer.Analyzer ()
analyzer = () <$ oneOf "\\:#,;\"()[]{}"
