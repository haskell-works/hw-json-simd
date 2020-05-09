{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Capabilities
  ( cmdCapabilities
  ) where

import HaskellWorks.Data.Json.Simd.Index.Simple
import HaskellWorks.Data.Json.Simd.Index.Standard
import Options.Applicative                        hiding (columns)

import qualified System.IO as IO

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

runCapabilities :: () -> IO ()
runCapabilities _ = do
  IO.putStrLn "Capabalities:"
  IO.putStrLn $ "  standard indexing: " <> show enabledMakeStandardJsonIbBps
  IO.putStrLn $ "  simple indexing: "   <> show enabledMakeSimpleJsonIbBps

optsCapabilities :: Parser ()
optsCapabilities = pure ()

cmdCapabilities :: Mod CommandFields (IO ())
cmdCapabilities = command "capabilities" $ flip info idm $ runCapabilities <$> optsCapabilities
