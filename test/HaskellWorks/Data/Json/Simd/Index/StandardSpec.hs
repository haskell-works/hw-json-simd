{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Simd.Index.StandardSpec
  ( spec
  ) where

import Data.Semigroup                 ((<>))
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Lazy                       as LBS
import qualified HaskellWorks.Data.ByteString.Lazy          as LBS
import qualified HaskellWorks.Data.Json.Simd.Index.Standard as SI

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

makeStandardJsonIbs :: LBS.ByteString -> BS.ByteString
makeStandardJsonIbs bs = let Right chunks = fmap fst <$> SI.makeStandardJsonIbBps (LBS.resegmentPadded 512 bs) in mconcat chunks

makeStandardJsonBps :: LBS.ByteString -> BS.ByteString
makeStandardJsonBps bs = let Right chunks = fmap snd <$> SI.makeStandardJsonIbBps (LBS.resegmentPadded 512 bs) in mconcat chunks

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.Cursor.InterestBitsSpec" $ do
  it "Evaluating interest bits" $ requireTest $ do
    bitShow (makeStandardJsonBps ""           ) === ""
    bitShow (makeStandardJsonBps "  \n \r \t ") === ""
    bitShow (makeStandardJsonBps "1234 "      ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "1.1 "       ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "-1.1e-1 "   ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "false "     ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "true "      ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "\"hello\" " ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "\"\\\"\" "  ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "{ "         ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "} "         ) === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "[ "         ) === "10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps "] "         ) === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps ": "         ) === ""
    bitShow (makeStandardJsonBps ", "         ) === ""
    bitShow (makeStandardJsonBps "{{}}"       ) === "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonBps " { { } } "  ) === "11000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "Evaluating interest bits" $ requireTest $ do
    bitShow (makeStandardJsonIbs ""           ) === ""
    bitShow (makeStandardJsonIbs "  \n \r \t ") === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "1234 "      ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "1.1 "       ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "-1.1e-2 "   ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "false "     ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "true "      ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "\"hello\" " ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "\"\\\"\" "  ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "{ "         ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "} "         ) === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "[ "         ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "] "         ) === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs ": "         ) === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs ", "         ) === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "{{}}"       ) === "11000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs " { { } } "  ) === "01010000" <> mconcat (replicate 63 " 00000000")
  it "Evaluating interest bits 2" $ requireTest $ do
    bitShow (makeStandardJsonIbs ""           ) === ""
    -- bitShow (makeStandardJsonIbs "  \n \r \t ") === "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    bitShow (makeStandardJsonIbs "1234 "      ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "1.1 "       ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "-1.1 "      ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "-1.1e-2 "   ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "false "     ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "true "      ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "\"hello\" " ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "\"\\\"\" "  ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "{ "         ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "} "         ) === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "[ "         ) === "10000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "] "         ) === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs ": "         ) === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs ", "         ) === "00000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs "{{}}"       ) === "11000000" <> mconcat (replicate 63 " 00000000")
    bitShow (makeStandardJsonIbs " { { } } "  ) === "01010000" <> mconcat (replicate 63 " 00000000")
