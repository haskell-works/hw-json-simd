{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Capabilities
  ( cmdCapabilities
  ) where

import Data.Semigroup      ((<>))
import Options.Applicative hiding (columns)

import qualified HaskellWorks.Data.Json.Simd.Index.Simple   as SIMPLE
import qualified HaskellWorks.Data.Json.Simd.Index.Standard as STANDARD
import qualified System.IO                                  as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runCapabilities :: () -> IO ()
runCapabilities opts = do
  IO.putStrLn "Capabalities:"
  IO.putStrLn $ "  standard indexing: " <> show STANDARD.enabled
  IO.putStrLn $ "  simple indexing: "   <> show SIMPLE.enabled

optsCapabilities :: Parser ()
optsCapabilities = pure ()

cmdCapabilities :: Mod CommandFields (IO ())
cmdCapabilities = command "capabilities" $ flip info idm $ runCapabilities <$> optsCapabilities
