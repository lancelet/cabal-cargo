module Main where

import           Cabgo                          ( RustProject(RustProject)
                                                , defaultMain
                                                , rustProjectDir
                                                , rustProjectIncludes
                                                , rustProjectLibs
                                                )

main :: IO ()
main = do
  let rustProject :: RustProject
      rustProject = RustProject { rustProjectDir      = "rustsrc"
                                , rustProjectLibs     = ["rustbits"]
                                , rustProjectIncludes = ["rustbits.h"]
                                }
  defaultMain rustProject
