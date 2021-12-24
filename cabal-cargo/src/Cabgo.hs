{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
module Cabgo
  ( RustProject(..)
  , defaultMain
  ) where

import           Cabgo.Cargo                    ( BuildType(Debug, Release)
                                                , cargoBuild
                                                )
import           Distribution.PackageDescription
                                                ( HookedBuildInfo
                                                , PackageDescription
                                                , emptyBuildInfo
                                                , includes
                                                )
import           Distribution.Simple            ( Args
                                                , DebugInfoLevel(NoDebugInfo)
                                                , UserHooks
                                                , buildHook
                                                , defaultMainWithHooks
                                                , hookedPrograms
                                                , preBuild
                                                , simpleUserHooks
                                                )
import           Distribution.Simple.LocalBuildInfo
                                                ( LocalBuildInfo
                                                , withDebugInfo
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
import           System.Directory               ( createDirectoryIfMissing
                                                , getCurrentDirectory
                                                )
import           System.FilePath                ( (</>) )


data RustProject = RustProject
  { rustProjectDir      :: FilePath
  , rustProjectLibs     :: [String]
  , rustProjectIncludes :: [String]
  }
  deriving stock (Eq, Show)


defaultMain :: RustProject -> IO ()
defaultMain proj = defaultMainWithHooks (cabgoUserHooks proj)


cabgoUserHooks :: RustProject -> UserHooks
cabgoUserHooks proj = simpleUserHooks { hookedPrograms = cabgoHookedPrograms
                                      , preBuild       = cabgoPreBuild proj
                                      , buildHook      = cabgoBuildHook proj
                                      }


cabgoHookedPrograms :: [Program]
cabgoHookedPrograms = [simpleProgram "cargo"]


cabgoPreBuild :: RustProject -> Args -> BuildFlags -> IO HookedBuildInfo
cabgoPreBuild RustProject {..} _ _ = do
  cwd <- getCurrentDirectory

  let releaseDir, debugDir :: FilePath
      releaseDir = cwd </> rustProjectDir </> "target" </> "release"
      debugDir   = cwd </> rustProjectDir </> "target" </> "debug"
  createDirectoryIfMissing True releaseDir
  createDirectoryIfMissing True debugDir

  -- Ideally, we would know if we were doing a release or debug build, but
  -- AFAICT, that information is not available here. So, instead, we add BOTH
  -- a release and debug directory for good measure.
  let buildInfo :: BuildInfo
      buildInfo = emptyBuildInfo { extraLibs    = rustProjectLibs
                                 , extraLibDirs = [releaseDir, debugDir]
                                 , includes     = rustProjectIncludes
                                 , includeDirs  = [cwd </> rustProjectDir]
                                 }

  pure (Just buildInfo, [])


cabgoBuildHook
  :: RustProject
  -> PackageDescription
  -> LocalBuildInfo
  -> UserHooks
  -> BuildFlags
  -> IO ()
cabgoBuildHook RustProject {..} packageDescription localBuildInfo userHooks buildFlags
  = do
    let buildType :: BuildType
        buildType = case withDebugInfo localBuildInfo of
          NoDebugInfo -> Release
          _           -> Debug

    putStrLn
      $  ">>> Starting cargo build in directory '"
      ++ rustProjectDir
      ++ "'..."
    cargoBuild (getCargoExe localBuildInfo) rustProjectDir buildType
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
