{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Commands.Types
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Semigroup      ((<>))
import Data.Word
import Foreign
import Options.Applicative hiding (columns)

import qualified App.Lens                          as L
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Internal          as BSI
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.ByteString      as BS
import qualified HaskellWorks.Data.ByteString.Lazy as LBS
import qualified System.IO                         as IO
import qualified System.IO.MMap                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

makeIndex :: LBS.ByteString -> [(B.Builder, B.Builder)]
makeIndex _ = []

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let filePath = opts ^. L.filePath
  let outputIbFile = opts ^. L.outputIbFile & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. L.outputBpFile & fromMaybe (filePath <> ".bp.idx")
  IO.withFile filePath IO.ReadMode $ \hIn -> do
    contents <- LBS.resegmentPadded 512 <$> LBS.hGetContents hIn
    let chunks = makeIndex contents
    IO.withFile outputIbFile IO.WriteMode $ \hIb -> do
      IO.withFile outputBpFile IO.WriteMode $ \hBp -> do
        forM_ chunks $ \(ibBuilder, bpBuilder) -> do
          B.hPutBuilder hIb ibBuilder
          B.hPutBuilder hBp bpBuilder

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

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
