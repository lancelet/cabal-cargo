{-# LANGUAGE CApiFFI #-}
-- |

module Bindings
  ( succInt8
  ) where

import           Data.Int                       ( Int8 )

foreign import capi "rustbits.h succ_i8" succInt8 :: Int8 -> Int8
