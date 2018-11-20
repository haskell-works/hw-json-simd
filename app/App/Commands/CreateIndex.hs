{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Commands.Types
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.Semigroup      ((<>))
import Data.Word
import Foreign
import Foreign.ForeignPtr
import Options.Applicative hiding (columns)

import qualified App.Lens                            as L
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Builder             as B
import qualified Data.ByteString.Internal            as BSI
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Lazy.Internal       as LBSI
import qualified Data.Vector.Storable                as DVS
import qualified Data.Vector.Storable.Mutable        as DVSM
import qualified Foreign.ForeignPtr                  as F
import qualified Foreign.ForeignPtr.Unsafe           as F
import qualified Foreign.Marshal.Unsafe              as F
import qualified Foreign.Ptr                         as F
import qualified HaskellWorks.Data.ByteString        as BS
import qualified HaskellWorks.Data.ByteString.Lazy   as LBS
import qualified HaskellWorks.Data.Json.Simd.Foreign as F
import qualified System.IO                           as IO
import qualified System.IO.MMap                      as IO
import qualified System.IO.Unsafe                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

-- processChunk :: ()
--   => Ptr UInt8   --  in_buffer
--   -> Size        --  in_length
--   -> Ptr UInt8   --  work_bits_of_d
--   -> Ptr UInt8   --  work_bits_of_a
--   -> Ptr UInt8   --  work_bits_of_z
--   -> Ptr UInt8   --  work_bits_of_q
--   -> Ptr UInt8   --  work_bits_of_b
--   -> Ptr UInt8   --  work_bits_of_e
--   -> Ptr Size    --  last_trailing_ones
--   -> Ptr Size    --  quote_odds_carry
--   -> Ptr Size    --  quote_evens_carry
--   -> Ptr UInt64  --  quote_mask_carry
--   -> Ptr UInt8   --  result_ibs
--   -> Ptr UInt8   --  result_a
--   -> Ptr UInt8   --  result_z
--   -> IO UInt64

data WorkBuffers = WorkBuffers
  { workBuffersP :: !(ForeignPtr F.UInt8)
  , workBuffersD :: !(Ptr F.UInt8)
  , workBuffersA :: !(Ptr F.UInt8)
  , workBuffersZ :: !(Ptr F.UInt8)
  , workBuffersQ :: !(Ptr F.UInt8)
  , workBuffersB :: !(Ptr F.UInt8)
  , workBuffersE :: !(Ptr F.UInt8)
  }

data WorkState = WorkState
  { workStateZ :: !(Ptr F.Size)
  , workStateO :: !(Ptr F.Size)
  , workStateE :: !(Ptr F.Size)
  , workStateM :: !(Ptr F.UInt64)
  , workStateP :: !(ForeignPtr Word8)
  }

allocWorkBuffers :: Int -> IO WorkBuffers
allocWorkBuffers n = do
  fptr <- F.mallocForeignPtrBytes (6 * n)
  let ptr = F.unsafeForeignPtrToPtr fptr
  return WorkBuffers
    { workBuffersP = fptr
    , workBuffersD = ptr `F.plusPtr` (n * 0)
    , workBuffersA = ptr `F.plusPtr` (n * 1)
    , workBuffersZ = ptr `F.plusPtr` (n * 2)
    , workBuffersQ = ptr `F.plusPtr` (n * 3)
    , workBuffersB = ptr `F.plusPtr` (n * 4)
    , workBuffersE = ptr `F.plusPtr` (n * 5)
    }

allocWorkState :: IO WorkState
allocWorkState = undefined

makeIndex :: LBS.ByteString -> [(BS.ByteString, BS.ByteString)]
makeIndex lbs = F.unsafeLocalState $ do
  wb <- allocWorkBuffers (32 * 1024 * 1204)
  ws <- allocWorkState
  IO.unsafeInterleaveIO $ go wb ws (LBS.toChunks lbs)
  where go :: WorkBuffers -> WorkState -> [BS.ByteString] -> IO [(BS.ByteString, BS.ByteString)]
        go wb ws []       = return [] -- TODO fill last bps
        go wb ws (bs:bss) = do
          let resLen = BS.length bs `div` 8
          resIb <- F.mallocForeignPtrBytes resLen
          resA  <- F.mallocForeignPtrBytes resLen
          resB  <- F.mallocForeignPtrBytes resLen
          _ <- F.processChunk
            undefined         --   => Ptr UInt8   --  in_buffer
            undefined         --   -> Size        --  in_length
            (workBuffersD wb) --   -> Ptr UInt8   --  work_bits_of_d
            (workBuffersA wb) --   -> Ptr UInt8   --  work_bits_of_a
            (workBuffersZ wb) --   -> Ptr UInt8   --  work_bits_of_z
            (workBuffersQ wb) --   -> Ptr UInt8   --  work_bits_of_q
            (workBuffersB wb) --   -> Ptr UInt8   --  work_bits_of_b
            (workBuffersE wb) --   -> Ptr UInt8   --  work_bits_of_e
            (workStateZ ws)   --   -> Ptr Size    --  last_trailing_ones
            (workStateO ws)   --   -> Ptr Size    --  quote_odds_carry
            (workStateE ws)   --   -> Ptr Size    --  quote_evens_carry
            (workStateM ws)   --   -> Ptr UInt64  --  quote_mask_carry
            undefined         --   -> Ptr UInt8   --  result_ibs
            undefined         --   -> Ptr UInt8   --  result_a
            undefined         --   -> Ptr UInt8   --  result_z
          return undefined

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
        forM_ chunks $ \(ibBs, bpBs) -> do
          BS.hPut hIb ibBs
          BS.hPut hBp bpBs

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
