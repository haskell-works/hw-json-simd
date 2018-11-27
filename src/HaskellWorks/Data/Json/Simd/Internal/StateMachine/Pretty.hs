{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Simd.Internal.StateMachine.Pretty
  ( transitionTableInC
  , transitionTableSimdInC
  , phiTableInC
  , phiTableSimdInC
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

instance PLit Word64 where
  plit w = fromString $ '0':'x':padl 8 '0' (showHex w [])

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
  where ws = DVS.toList SM.transitionTableSimd

phiTableSimdInC :: Doc ()
phiTableSimdInC = embraceN (chunksOf 8 (fmap plit ws))
  where ws = DVS.toList SM.transitionTableSimd

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
