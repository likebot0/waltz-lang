module Semantic.Expression where

import Global
import Context.Funs
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.HashMap.Strict
import qualified Diagnostic
import qualified Diagnostic.CannotResolve
import qualified Diagnostic.IndexOutOfBounds
import qualified Diagnostic.MismatchedTypes
import qualified Diagnostic.UnknownField
import qualified Error.CannotResolve
import qualified Semantic.Analyzer
import qualified Semantic.BlockExpression
import qualified Semantic.GroupedExpression
import qualified Semantic.Identifier
import qualified Semantic.LambdaExpression
import qualified Semantic.Literal.Array
import qualified Semantic.Literal.Number
import qualified Semantic.Literal.Object
import qualified Semantic.Literal.String
import qualified Semantic.TypeExpression

analyze :: Semantic.Analyzer.Analyze "expression"
analyze x = do
    termRef <- ref Nothing

    children <- mapM
        (\child -> do
            (ast, location, nextTerm) <- child |>
                do \(x :: Ast.Node "syntax-analyzed" "block-expression") -> do
                    y <- Semantic.BlockExpression.analyze x

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , inject $ Ast.Semantic.Type @ "unknown" ()
                        )
                @>
                do \(x :: Ast.Node "syntax-analyzed" "grouped-expression") -> do
                    y <- Semantic.GroupedExpression.analyze x

                    let expression = Ast.children y

                    let Ast.Semantic.ExpressionAttributes _ t = Ast.attributes expression

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , t
                        )
                @>
                do \(x :: Ast.Node "syntax-analyzed" "identifier") -> do
                    do
                        call @ "resolve" $ Ast.children x |> \case
                            '\'' : identifier -> identifier
                            identifier -> identifier

                        pure ()

                        |> withRaiseHandler (\e -> do
                            call @ "diagnostic/send" $ Diagnostic.Diagnostic
                                do inject $ Proxy @ "error"
                                do Ast.Syntax.location $ Ast.attributes x
                                do show e
                        )

                    y <- Semantic.Identifier.analyze x

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , inject $ Ast.Semantic.Type @ "unknown" ()
                        )
                @>
                do \(x :: Ast.Node "syntax-analyzed" "lambda-expression") -> do
                    y <- Semantic.LambdaExpression.analyze x

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , inject $ Ast.Semantic.Type @ "function" $ ""
                        )
                @>
                do \(x :: Ast.Node "syntax-analyzed" "literal/array") -> do
                    y <- Semantic.Literal.Array.analyze x

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , inject $ Ast.Semantic.Type @ "array" $ Ast.children y >>=
                            do \(x :: Ast.Node "semantic-analyzed" "expression") -> do
                                let Ast.Semantic.ExpressionAttributes _ t = Ast.attributes x

                                [t]
                            @>
                            do const []
                        )
                @>
                do \(x :: Ast.Node "syntax-analyzed" "literal/number") -> do
                    y <- Semantic.Literal.Number.analyze x

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , inject $ Ast.Semantic.Type @ "number" $ Ast.children y
                        )
                @>
                do \(x :: Ast.Node "syntax-analyzed" "literal/object") -> do
                    y <- Semantic.Literal.Object.analyze x

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , inject $ Ast.Semantic.Type @ "object" $ undefined
                        )
                @>
                do \(x :: Ast.Node "syntax-analyzed" "literal/string") -> do
                    y <- Semantic.Literal.String.analyze x

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , inject $ Ast.Semantic.Type @ "string" $ ""
                        )
                @>
                do \(x :: Ast.Node "syntax-analyzed" "type-expression") -> do
                    y <- Semantic.TypeExpression.analyze x

                    pure
                        ( inject y
                        , Ast.Syntax.location $ Ast.attributes x
                        , inject $ Ast.Semantic.Type @ "unknown" ()
                        )
                @>
                typesExhausted

            result <- get termRef >>= \case
                Nothing -> pure nextTerm
                Just prevTerm -> apply prevTerm nextTerm location

            set termRef $ Just result

            pure ast
        )
        do Ast.children x

    term <- get termRef

    pure $ Ast.Node children
        do Ast.Semantic.ExpressionAttributes
            do Ast.Syntax.location $ Ast.attributes x
            do term |> \case
                Nothing -> undefined
                Just x -> x

apply x y l = x |>
    do \(x :: Ast.Semantic.Type "array") -> run do
        y <- y |> restrict @ (Ast.Semantic.Type "unknown") |> \case
            Left y -> pure y
            Right y -> do
                return $ inject y

        y <- y |> restrict @ (Ast.Semantic.Type "number") |> \case
            Right y -> pure y
            Left y -> do
                Diagnostic.MismatchedTypes.send l

                return $ inject $ Ast.Semantic.Type @ "unknown" ()

        let Ast.Semantic.Type text = y

        let rawIndex = read text :: Double

        let index = ceiling rawIndex :: Int

        let Ast.Semantic.Type elements = x

        when (fromIntegral index /= rawIndex) do
            Diagnostic.MismatchedTypes.send l

            return $ inject $ Ast.Semantic.Type @ "unknown" ()

        let n = length elements

        when (index < 0 || index >= n) do
            Diagnostic.IndexOutOfBounds.send l (show index) n

            return $ inject $ Ast.Semantic.Type @ "unknown" ()

        return $ elements !! index
    @>
    do \(x :: Ast.Semantic.Type "function") -> run do
        return y
    @>
    do \(x :: Ast.Semantic.Type "number") -> run do
        y |> restrict @ (Ast.Semantic.Type "unknown") |> \case
            Right _ -> pure ()
            Left y -> do
                Diagnostic.MismatchedTypes.send l

        return $ inject $ Ast.Semantic.Type @ "unknown" ()
    @>
    do \(x :: Ast.Semantic.Type "object") -> run do
        y <- y |> restrict @ (Ast.Semantic.Type "unknown") |> \case
            Left y -> pure y
            Right y -> do
                return $ inject y

        y <- y |> restrict @ (Ast.Semantic.Type "string") |> \case
            Right y -> pure y
            Left y -> do
                Diagnostic.MismatchedTypes.send l

                return $ inject $ Ast.Semantic.Type @ "unknown" ()

        let Ast.Semantic.Type map = x

        let Ast.Semantic.Type key = y

        Data.HashMap.Strict.lookup key map |> \case
            Just x -> pure x
            Nothing -> do
                Diagnostic.UnknownField.send l key

                return $ inject $ Ast.Semantic.Type @ "unknown" ()
    @>
    do \(x :: Ast.Semantic.Type "string") -> run do
        y <- y |> restrict @ (Ast.Semantic.Type "unknown") |> \case
            Left y -> pure y
            Right _ -> do
                return $ inject $ Ast.Semantic.Type @ "unknown" ()

        y <- y |> restrict @ (Ast.Semantic.Type "number") |> \case
            Right y -> pure y
            Left y -> do
                Diagnostic.MismatchedTypes.send l

                return $ inject $ Ast.Semantic.Type @ "unknown" ()

        let Ast.Semantic.Type text = y

        let rawIndex = read text :: Double

        let index = ceiling rawIndex :: Int

        when (fromIntegral index /= rawIndex) do
            Diagnostic.MismatchedTypes.send l

            return $ inject $ Ast.Semantic.Type @ "unknown" ()

        let Ast.Semantic.Type string = x

        let n = length string

        when (index < 0 || index >= n) do
            Diagnostic.IndexOutOfBounds.send
                l
                do show index
                n

            return $ inject $ Ast.Semantic.Type @ "unknown" ()

        return $ inject $ Ast.Semantic.Type @ "string" $ [string !! index]
    @>
    do \(x :: Ast.Semantic.Type "unknown") -> run do
        return $ inject x
    @>
    typesExhausted
