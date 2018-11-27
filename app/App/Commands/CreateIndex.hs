{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Commands.Types
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Semigroup      ((<>))
import Options.Applicative hiding (columns)

import qualified App.Lens                                   as L
import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Lazy                       as LBS
import qualified HaskellWorks.Data.ByteString.Lazy          as LBS
import qualified HaskellWorks.Data.Json.Simd.Index.Simple   as SIMPLE
import qualified HaskellWorks.Data.Json.Simd.Index.Standard as STANDARD
import qualified System.Exit                                as IO
import qualified System.IO                                  as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

zipPadded :: a -> b -> [a] -> [b] -> [(a, b)]
zipPadded a b (c:cs) (d:ds) = (c, d):zipPadded a b cs ds
zipPadded a b []     (d:ds) = (a, d):zipPadded a b [] ds
zipPadded a b (c:cs) []     = (c, b):zipPadded a b cs []
zipPadded _ _ []     []     = []

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let filePath      = opts ^. L.filePath
  let outputIbFile  = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile  = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  let method        = opts ^. L.method

  case method of
    "simple" -> do
      IO.withFile filePath IO.ReadMode $ \hIn -> do
        contents <- LBS.resegmentPadded 512 <$> LBS.hGetContents hIn
        let chunks = SIMPLE.makeIbs contents
        let chunks2 = zipPadded BS.empty BS.empty (fmap (\(a, _, _) -> a) chunks) (SIMPLE.ibsToIndexByteStrings chunks)
        IO.withFile outputIbFile IO.WriteMode $ \hIb -> do
          IO.withFile outputBpFile IO.WriteMode $ \hBp -> do
            forM_ chunks2 $ \(ibBs, bpBs) -> do
              BS.hPut hIb ibBs
              BS.hPut hBp bpBs
    "standard" -> do
      IO.withFile filePath IO.ReadMode $ \hIn -> do
        contents <- LBS.resegmentPadded 512 <$> LBS.hGetContents hIn
        let chunks = STANDARD.makeIbBps contents
        IO.withFile outputIbFile IO.WriteMode $ \hIb -> do
          IO.withFile outputBpFile IO.WriteMode $ \hBp -> do
            forM_ chunks $ \(ibBs, bpBs) -> do
              BS.hPut hIb ibBs
              BS.hPut hBp bpBs
    _ -> do
      IO.hPutStrLn IO.stderr $ "Unrecognised method: " <> show method
      IO.exitFailure

optsCreateIndex :: Parser CreateIndexOptions
optsCreateIndex = CreateIndexOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "STRING"
        )
  <*> optional
        ( strOption
          (   long "output-ib-file"
          <>  help "Filename for output ib index"
          <>  metavar "STRING"
          )
        )
  <*> optional
        ( strOption
          (   long "output-bp-file"
          <>  help "Filename for output bp index"
          <>  metavar "STRING"
          )
        )
  <*> strOption
        (   long "method"
        <>  help "Method"
        <>  value "simple"
        <>  metavar "STRING"
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
