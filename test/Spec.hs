{-# LANGUAGE OverloadedStrings #-}

import qualified HaskellWorks.Data.ByteString.Lazy          as LBS
import           HaskellWorks.Data.Json.Simd.Index.Standard

main :: IO ()
main = do
  -- let res = makeStandardJsonIbBps "{}"
  let res = makeStandardJsonIbBps . LBS.resegmentPadded 512 $ "{}"
  case res of
    Right chunks -> do
      putStrLn $ "Chunks:"
      let triggerBug = True
      if triggerBug then putStrLn $ show (length chunks) else pure ()

    err ->
        putStrLn $ "No chunks: " ++ show err
