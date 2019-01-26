{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Capabilities
  ( cmdCapabilities
  ) where

import Data.Semigroup                             ((<>))
import HaskellWorks.Data.Json.Simd.Index.Simple
import HaskellWorks.Data.Json.Simd.Index.Standard
import Options.Applicative                        hiding (columns)

import qualified System.IO as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runCapabilities :: () -> IO ()
runCapabilities opts = do
  IO.putStrLn "Capabalities:"
  IO.putStrLn $ "  standard indexing: " <> show enabledMakeStandardJsonIbBps
  IO.putStrLn $ "  simple indexing: "   <> show enabledMakeSimpleJsonIbBps

optsCapabilities :: Parser ()
optsCapabilities = pure ()

cmdCapabilities :: Mod CommandFields (IO ())
cmdCapabilities = command "capabilities" $ flip info idm $ runCapabilities <$> optsCapabilities
