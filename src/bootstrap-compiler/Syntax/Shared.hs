module Syntax.Shared where

import Global
import Context.Funs
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Diagnostic
import qualified Syntax.Analyzer
import qualified Syntax.Diagnostic.Position
import qualified Syntax.Punctuation
import qualified Syntax.UnexpectedEndOfInput
import qualified Syntax.UnexpectedToken
import qualified Syntax.Whitespace

node :: Syntax.Analyzer.Hoa (Ast.Children "syntax-analyzed" a) (Ast.Node "syntax-analyzed" a)
node p = do
    start <- Syntax.Diagnostic.Position.analyzer

    x <- p

    end <- Syntax.Diagnostic.Position.analyzer

    pure $ Ast.Node
        do Ast.Syntax.Attributes do Diagnostic.Location start end
        x

keyword k = try do
    x <- chunk k

    lookAhead $ choice
        [ eof
        , Syntax.Punctuation.analyzer
        , Syntax.Whitespace.analyzer
        ]

    pure x

between begin end p skip = do
    begin
    go
    where
        go = choice
            [ [] <$ end
            , [] <$ Syntax.UnexpectedEndOfInput.analyzer
            , (:) <$> p <*> go
            , skip *> go
            , Syntax.UnexpectedToken.analyzer *> go
            ]
{-# INLINE between #-}

skipManyTill p end = do
    Text.Megaparsec.skipManyTill
        do p
        do choice
            [ () <$ end
            , Syntax.UnexpectedEndOfInput.analyzer
            ]
