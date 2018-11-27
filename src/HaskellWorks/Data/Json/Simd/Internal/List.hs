module HaskellWorks.Data.Json.Simd.Internal.List
  ( padl
  ) where

padl :: Int -> a -> [a] -> [a]
padl n c s = replicate ((n - length s) `max` 0) c <> s
