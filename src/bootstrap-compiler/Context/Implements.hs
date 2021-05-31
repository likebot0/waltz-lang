module Context.Implements where

import Global
import qualified Action.GetCurrentSrcId
import qualified Action.GetEntrypoints
import qualified Action.GetOptDirs
import qualified Action.GetOutDir
import qualified Action.GetPathById
import qualified Action.GetSrcDirs
import qualified Action.Resolve
import qualified Ast.Dump
import qualified Ast.DumpJson
import qualified Fs.ReadFile

withContext x = x
    |> Action.GetCurrentSrcId.withDefault
    |> Action.GetEntrypoints.withDefault
    |> Action.GetOptDirs.withDefault
    |> Action.GetOutDir.withDefault
    |> Action.GetPathById.withDefault
    |> Action.GetSrcDirs.withDefault
    |> Action.Resolve.withDefault
    |> Ast.Dump.withDefault
    |> Ast.DumpJson.withDefault
    |> Fs.ReadFile.withDefault
    |> runM
