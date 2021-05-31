module Main where

import Global
import Context.Funs
import Context.Implements
import qualified System.IO

main :: IO ()
main = do
    entryPoints <- call @ "get-entrypoints" ()

    (`mapM_` entryPoints) $ \entryPoint -> do
        ast <- call @ "resolve" entryPoint

        pure ()

    pure ()

    |> with @ "diagnostic/send" do
        fun \x -> do
            path <- call @ "get-current-src-path" ()

            sendM $ System.IO.hPutStrLn System.IO.stderr $ path ++ show x

    |> withRaiseHandler (\x -> do
        sendM $ System.IO.hPrint System.IO.stderr x
    )
    |> withContext
