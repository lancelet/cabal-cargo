module Main
  ( main
  ) where

import           Bindings                       ( succInt8 )
import           Data.Int                       ( Int8 )

main :: IO ()
main = do
  putStrLn "Hello World test-project"
  let x :: Int8
      x = 41
  let y = succInt8 x
  putStrLn $ "x = " ++ show x
  putStrLn $ "y = " ++ show y
