{-# LANGUAGE OverloadedStrings #-}
module Cabgo
  ( defaultMain
  ) where

import           Cabgo.Cargo                    ( cargoBuild )
import           Distribution.PackageDescription
                                                ( HookedBuildInfo
                                                , PackageDescription
                                                , emptyBuildInfo
                                                , includes
                                                )
import           Distribution.Simple            ( Args
                                                , UserHooks
                                                , buildHook
                                                , defaultMainWithHooks
                                                , hookedPrograms
                                                , preBuild
                                                , simpleUserHooks
                                                )
import           Distribution.Simple.LocalBuildInfo
                                                ( LocalBuildInfo
                                                , withPrograms
                                                )
import           Distribution.Simple.Program    ( Program
                                                , lookupProgram
                                                , programPath
                                                , simpleProgram
                                                )
import           Distribution.Simple.Setup      ( BuildFlags )
import           Distribution.Types.BuildInfo   ( BuildInfo
                                                , extraLibDirs
                                                , extraLibs
                                                , includeDirs
                                                )
import           System.Directory               ( getCurrentDirectory )
import           System.FilePath                ( (</>) )


type RustProjectDir = FilePath


defaultMain :: RustProjectDir -> IO ()
defaultMain projDir = defaultMainWithHooks (cabgoUserHooks projDir)


cabgoUserHooks :: RustProjectDir -> UserHooks
cabgoUserHooks projDir = simpleUserHooks { hookedPrograms = cabgoHookedPrograms
                                         , preBuild = cabgoPreBuild projDir
                                         , buildHook = cabgoBuildHook projDir
                                         }


cabgoHookedPrograms :: [Program]
cabgoHookedPrograms = [simpleProgram "cargo"]


cabgoPreBuild :: RustProjectDir -> Args -> BuildFlags -> IO HookedBuildInfo
cabgoPreBuild projDir _ _ = do
  cwd <- getCurrentDirectory
  let buildInfo :: BuildInfo
      buildInfo = emptyBuildInfo
        { extraLibs    = ["rustbits"]
        , extraLibDirs = [
              cwd </> projDir </> "target" </> "release"
            , cwd </> projDir </> "target" </> "debug"
        ]
        , includes     = ["rustbits.h"]
        , includeDirs  = [cwd </> projDir]
        }
  pure (Just buildInfo, [])


cabgoBuildHook
  :: RustProjectDir
  -> PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
cabgoBuildHook projDir packageDescription localBuildInfo userHooks buildFlags =
  do
    let cargoExe :: FilePath
        cargoExe = getCargoExe localBuildInfo

    putStrLn $ ">>> Starting cargo build in directory '" ++ projDir ++ "'..."
    cargoBuild cargoExe projDir
    putStrLn ">>> Finished cargo build."

    buildHook simpleUserHooks
              packageDescription
              localBuildInfo
              userHooks
              buildFlags


getCargoExe :: LocalBuildInfo -> FilePath
getCargoExe localBuildInfo =
  case lookupProgram (simpleProgram "cargo") (withPrograms localBuildInfo) of
    Nothing ->
      error
        $  "cabal could not find the 'cargo' executable. Have you "
        ++ "installed the Rust toolchain and configured the path?"
    Just x -> programPath x
