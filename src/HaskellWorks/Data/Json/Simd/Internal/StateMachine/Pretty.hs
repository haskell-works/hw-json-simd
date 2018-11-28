{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Simd.Internal.StateMachine.Pretty
  ( transitionTableInC
  , transitionTableSimdInC
  , phiTableInC
  , phiTableSimdInC
  , transitionPhiTableSimdInC
  ) where

import Data.List
import Data.List.Split
import Data.String
import Data.Text.Prettyprint.Doc
import Data.Word
import HaskellWorks.Data.Json.Simd.Internal.List
import Numeric
import Prelude                                   hiding ((<>))

import qualified Data.Vector                                       as DV
import qualified Data.Vector.Storable                              as DVS
import qualified HaskellWorks.Data.Json.Simd.Internal.StateMachine as SM

class PLit a where
  plit :: a -> Doc ()

instance PLit Word8 where
  plit w = fromString $ '0':'x':padl 2 '0' (showHex w [])

instance PLit Word32 where
  plit w = fromString $ '0':'x':padl 8 '0' (showHex w [])

instance PLit Word64 where
  plit w = fromString $ '0':'x':padl 16 '0' (showHex w [])

instance (PLit a, PLit b, PLit c, PLit d) => PLit (Vec4 a b c d) where
  plit (Vec4 a b c d) = "{"
    <> plit a <> ", "
    <> plit b <> ", "
    <> plit c <> ", "
    <> plit d <> "}"

instance (PLit a, PLit b) => PLit (Vec2 a b) where
  plit (Vec2 a b) = "{"
    <> plit a <> ", "
    <> plit b <> "}"

instance (PLit WZero) where
  plit _ = "0"

transitionTableInC :: Doc ()
transitionTableInC = embrace (mkV <$> wss)
  where wss = DVS.toList <$> DV.toList SM.transitionTable
        mkV :: [Word8] -> Doc ()
        mkV ws = embraceN (chunksOf 16 (fmap plit ws))

phiTableInC :: Doc ()
phiTableInC = embrace (mkV <$> wss)
  where wss = DVS.toList <$> DV.toList SM.phiTable
        mkV :: [Word8] -> Doc ()
        mkV ws = embraceN (chunksOf 16 (fmap plit ws))

transitionTableSimdInC :: Doc ()
transitionTableSimdInC = embraceN (chunksOf 8 (fmap plit ws))
  where ws = word32 . fromIntegral <$> DVS.toList SM.transitionTableSimd

phiTableSimdInC :: Doc ()
phiTableSimdInC = embraceN (chunksOf 8 (fmap plit ws))
  where ws = word32 . fromIntegral <$> DVS.toList SM.phiTableSimd

embrace :: [Doc ()] -> Doc ()
embrace (d:ds) = intro "{" d <> mconcat (intro "," <$> ds) <> "}"
  where intro p e = p <> " " <> nest 2 e <> line
embrace [] = "{}"

embraceN :: [[Doc ()]] -> Doc ()
embraceN (ds:dss) = vsep $ concat
  [ ["{ " <> mconcat (intersperse ", " ds)]
  , (", " <>) . mconcat . intersperse ", " <$> dss
  , ["}"]
  ]
embraceN [] = "{}"

data Vec4 a b c d = Vec4 a b c d

data Vec2 a b = Vec2 a b

data WZero = WZero

word64 :: Word64 -> Word64
word64 = id

word32 :: Word32 -> Word32
word32 = id

transitionPhiTableSimdWideInC :: Doc ()
transitionPhiTableSimdWideInC = embraceN (chunksOf 1 (fmap plit pts))
  where pts = zipWith (\p t -> Vec4 t WZero p WZero)
                  (word64 . fromIntegral <$>  DVS.toList SM.phiTableSimd        )
                  (                           DVS.toList SM.transitionTableSimd )

transitionPhiTableSimdInC :: Doc ()
transitionPhiTableSimdInC = embraceN (chunksOf 1 (fmap plit pts))
  where pts = zipWith (\p t -> Vec2 t p)
                  (word64 . fromIntegral <$>  DVS.toList SM.phiTableSimd        )
                  (                           DVS.toList SM.transitionTableSimd )
