{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Cabgo.Cargo
  ( cargoBuild
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

-- | Run @cargo build@.
cargoBuild
  :: FilePath  -- ^ Path to the @cargo@ executable.
  -> FilePath  -- ^ Working directory for the @cargo@ command.
  -> IO ()
cargoBuild cargoExe cargoCwd = do
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
    else error $ "cargo build failed; exit code: " ++ show exitCode

-- | Run @cargo clean@.
cargoClean :: FilePath -> FilePath -> IO ()
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
