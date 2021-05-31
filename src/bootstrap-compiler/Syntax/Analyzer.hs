module Syntax.Analyzer where

import Global
import Context.Funs
import Data.Void
import qualified Text.Megaparsec

type ParsecT = Text.Megaparsec.ParsecT Void String

type Analyzer a = forall e. Constraints e => ParsecT (Eff e) a

type Hoa a b = forall e. Constraints e => ParsecT (Eff e) a -> ParsecT (Eff e) b

type WithEnd a = forall e. Constraints e => String -> ParsecT (Eff e) a

type Constraints e =
    ( Member (UseContext "diagnostic/send") e
    , Members (Require "diagnostic/send") e
    , Members Implemented e
    , Member Raise e
    , LastMember IO e
    )
