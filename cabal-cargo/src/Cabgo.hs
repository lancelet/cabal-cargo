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


-- | Defines a Rust project.
data RustProject = RustProject
  { -- | Subdirectory of the project containing the Rust project.
    --
    -- eg. @"rustSrc"@
    rustProjectDir      :: FilePath
    -- | List of libraries produced by the Rust project.
  , rustProjectLibs     :: [String]
    -- | List of include files (C headers) in the top level directory of the
    --   Rust project.
  , rustProjectIncludes :: [String]
  }
  deriving stock (Eq, Show)


-- | Main entry point.
--
-- Call this from @main@ in your @Setup.hs@ file.
defaultMain :: RustProject -> IO ()
defaultMain proj = defaultMainWithHooks (cabgoUserHooks proj)


-- | User hooks to be installed to build the Rust project and link it to the
--   Haskell binaries.
cabgoUserHooks :: RustProject -> UserHooks
cabgoUserHooks proj = simpleUserHooks { hookedPrograms = cabgoHookedPrograms
                                      , preBuild       = cabgoPreBuild proj
                                      , buildHook      = cabgoBuildHook proj
                                      }


-- | Hook to use @cabal@ to search for the @cargo@ executable.
cabgoHookedPrograms :: [Program]
cabgoHookedPrograms = [simpleProgram "cargo"]


-- | PreBuild hook.
--
-- This hook does the following:
--
--   1. Adds the release and debug build directories from the Rust project to
--      the @extra-lib-dirs@ build info parameter. This allows @cabal@ to find
--      the compiled libraries from the Rust project.
--
--   2. Adds the Rust library(ies) to the @extra-libs@ build info parameter.
--      This specifies that the Rust library(ies) should be linked by @cabal@.
--
--   3. Adds the Rust includes to the @includes@ build info parameter. This
--      allows the Haskell project to use @capi@ foreign imports by including
--      the C header files from the Rust project.
--
--   4. Adds the Rust project directory to the @include-dirs@ build info
--      parameter. This allows the include files to be found by @cabal@.
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


-- | Build hook.
--
-- This calls @cargo build@ to build the Rust project, and then calls the
-- default build hook.
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


-- | Get the path to the @cargo@ executable.
getCargoExe :: LocalBuildInfo -> FilePath
getCargoExe localBuildInfo =
  case lookupProgram (simpleProgram "cargo") (withPrograms localBuildInfo) of
    Nothing ->
      error
        $  "cabal could not find the 'cargo' executable. Have you "
        ++ "installed the Rust toolchain and configured the path?"
    Just x -> programPath x
