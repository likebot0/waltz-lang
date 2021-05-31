module Action.GetPathById where

import Global
import Context.Funs
import qualified Error.CannotResolve
import qualified System.Directory

withDefault = with @ "get-path-by-id" $ fun \identifier -> do
    do
        identifier <- identifier |> \case
            '~' : '/' : xs -> pure xs
            x -> pure x

        srcDirs <- call @ "get-src-dirs" ()

        sendM (System.Directory.findFile ((++ identifier) <$> srcDirs) ".wz") >>= \case
            Just x -> return $ normalize x
            _ -> pure ()

        optDirs <- call @ "get-opt-dirs" ()

        optSrcDirs <- pure do
            optDir <- optDirs

            srcDir <- srcDirs

            pure $ optDir ++ "/" ++ srcDir

        sendM (System.Directory.findFile ((++ identifier) <$> optSrcDirs) ".wz") >>= \case
            Just x -> return $ normalize x
            _ -> pure ()

    raise $ Error.CannotResolve.Type identifier

normalize :: String -> String
normalize = ((\x -> if x == '\\' then '/' else x) <$>)
