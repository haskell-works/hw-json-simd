{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HaskellWorks.Data.Json.Simd.Fsm.Core
  ( -- * Building state machines
    buildStateMachine
  , State(..)
  , Transition(..)
  , StateMachine(..)

    -- * Running state machines
  , run
  ) where

import Data.Binary     (Binary (..))
import Data.ByteString (ByteString)
import Data.Vector     ((!))
import Data.Word       (Word8)
import Foreign         (Ptr)
import Foreign.C.Types (CChar (..), CSize (..))
import GHC.Generics    (Generic)

import qualified Control.Parallel.Strategies
import qualified Data.Binary
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Unsafe
import qualified Data.Vector
import qualified Foreign
import qualified Foreign.Marshal.Unsafe

data State
  = S00
  | S01
  | S02
  | S03
  | S04
  | S05
  | S06
  | S07
  | S08
  | S09
  | S10
  | S11
  | S12
  | S13
  | S14
  | S15
  deriving (Binary, Bounded, Enum, Eq, Generic, Ord, Show)

numberOfStates :: Int
numberOfStates = fromEnum (maxBound :: State) + 1

newtype Transition = Transition { runTransition :: State -> State }

instance Monoid Transition where
  mempty = Transition id

  mappend (Transition f) (Transition g) = Transition (g . f)

instance Binary Transition where
  put (Transition f) = mapM_ (put . f) [minBound..maxBound]

  get = do
      !ss <- Data.Vector.replicateM numberOfStates get
      return (Transition (\s -> ss ! fromEnum s))

-- | A `StateMachine` is a function from a byte (i.e. `Word8`) to a `Transition`
newtype StateMachine = StateMachine { runStateMachine :: Word8 -> Transition }

instance Binary StateMachine where
  put (StateMachine k) = mapM_ (put . k) [minBound..maxBound]

  get = do
      let numBytes = fromEnum (maxBound :: Word8) + 1
      ts <- Data.Vector.replicateM numBytes get
      return (StateMachine (\word8 -> ts ! fromEnum word8))

{-| Convenient utility to build a `StateMachine` from a function of two
  arguments
-}
buildStateMachine :: (Word8 -> State -> State) -> StateMachine
buildStateMachine f = StateMachine (fmap Transition f)

foreign import ccall "run" c_run
  :: Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO ()

{-| Wrap the @c_run@ function in a Haskell API
  prop> runSerial (StateMachine f) bytes == foldMap f (Data.ByteString.unpack bytes)
-}
runSerial :: StateMachine -> ByteString -> Transition
runSerial matrix bytes = Data.Binary.decode (Data.ByteString.Lazy.fromStrict (
  Foreign.Marshal.Unsafe.unsafeLocalState (do
      Data.ByteString.Unsafe.unsafeUseAsCStringLen tBytes (\(ptrTBytes, _) ->
          Data.ByteString.Unsafe.unsafeUseAsCStringLen bytes (\(ptrIn, len) ->
              Foreign.allocaBytes numberOfStates (\ptrOut -> do
                  c_run ptrIn (fromIntegral len) ptrTBytes ptrOut
                  Data.ByteString.packCStringLen (ptrOut, numberOfStates) ) ) ) ) ))
  where tBytes = Data.ByteString.Lazy.toStrict (Data.Binary.encode matrix)

-- | Split a `ByteString` into chunks of size @n@
chunkBytes :: Int -> ByteString -> [ByteString]
chunkBytes n bytes =
  if Data.ByteString.null bytes
  then []
  else prefix : chunkBytes n suffix
  where ~(prefix, suffix) = Data.ByteString.splitAt n bytes

{-| Run a `StateMachine` on a `ByteString`
  `run` returns a `Transition` that computes what the final state would be for
  each possible initial state
  The implementation is equivalent to:
  prop> run n (StateMachine f) bytes == foldMap f (Data.ByteString.unpack bytes)
  ... except much more efficient and parallel
  The first argument specifies how many threads to use to accelerate the
  computation.  A good rule of thumb is to use the number of cores your
  machine has, like this:
  > ...
  > numCores <- Control.Concurrent.getNumCapabilities
  > let transition = run numCores stateMachine bytes
  > ...
  ... or you can just specify @1@ thread for a serial implementation (which
  will still be really efficient)
  `run` is \"embarassingly parallel\", meaning that the performance scales
  linearly with the number of available cores
-}
run :: Int
  -- ^ Number of threads to use
  -> StateMachine
  -- ^ State machine to run over the input bytes
  -> ByteString
  -- ^ Input bytes to feed to the state machine
  -> Transition
  -- ^ Computed function from every starting state to every final state
run 1          matrix bytes = runSerial matrix bytes
run numThreads matrix bytes =
  mconcat
      (Control.Parallel.Strategies.parMap
          Control.Parallel.Strategies.rseq
          (runSerial matrix)
          (chunkBytes subLen bytes) )
  where len = Data.ByteString.length bytes
        subLen = ((len - 1) `div` numThreads) + 1
