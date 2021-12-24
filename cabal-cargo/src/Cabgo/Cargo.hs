{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Cabgo.Cargo
  ( BuildType(Debug, Release)
  , cargoBuild
  , cargoClean
  ) where

import           System.Exit                    ( ExitCode(ExitSuccess) )
import           System.Process                 ( CreateProcess
                                                  ( cwd
                                                  , std_err
                                                  , std_in
                                                  , std_out
                                                  )
                                                , StdStream(Inherit, NoStream)
                                                , proc
                                                , waitForProcess
                                                , withCreateProcess
                                                )


-- | Type of build for @cargo@ to perform.
data BuildType
  = Debug
  | Release
  deriving stock (Eq, Show)


-- | Run @cargo build@.
cargoBuild
  :: FilePath   -- ^ Path to the @cargo@ executable.
  -> FilePath   -- ^ Working directory for the @cargo@ command.
  -> BuildType  -- ^ Whether to do a debug or release build.
  -> IO ()
cargoBuild cargoExe cargoCwd buildType = do
  let profileFlags :: [String]
      profileFlags = case buildType of
        Debug   -> ["--profile", "dev"]
        Release -> ["--profile", "release"]

      createProcess :: CreateProcess
      createProcess = (proc cargoExe ("build" : profileFlags))
        { cwd     = Just cargoCwd
        , std_in  = NoStream
        , std_out = Inherit
        , std_err = Inherit
        }

  exitCode <- withCreateProcess createProcess
    $ \_ _ _ processHandle -> waitForProcess processHandle

  if exitCode == ExitSuccess
    then pure ()
    else error $ "cargo build failed; exit code: " ++ show exitCode


-- | Run @cargo clean@.
cargoClean
  :: FilePath  -- ^ Path to the @cargo@ executable.
  -> FilePath  -- ^ Working directory for the @cargo@ command.
  -> IO ()
cargoClean cargoExe cargoCwd = do
  let createProcess :: CreateProcess
      createProcess = (proc cargoExe ["build"]) { cwd     = Just cargoCwd
                                                , std_in  = NoStream
                                                , std_out = Inherit
                                                , std_err = Inherit
                                                }
  exitCode <- withCreateProcess createProcess
    $ \_ _ _ processHandle -> waitForProcess processHandle

  if exitCode == ExitSuccess
    then pure ()
    else error $ "cargo clean failed; exit code: " ++ show exitCode
