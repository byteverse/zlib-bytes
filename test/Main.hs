{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

import Control.Monad (forM_)
import Data.Bytes (Bytes)
import System.Exit (exitFailure)
import Zlib (ZlibError (DataCorrupt), decompress)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.List as List
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Primitive.PrimArray as Prim
import qualified GHC.Exts as Exts

main :: IO ()
main = do
  forM_ files (uncurry oneTest)
  let garbage = Exts.fromList (List.replicate 300 0xFF) :: Bytes
  case decompress garbage of
    Left DataCorrupt -> pure ()
    Left e -> fail ("unexpected error " ++ show e)
    Right _ -> fail "garbage data should decompress successfully"
 where
  files =
    [ ("test/expected-001.txt", "test/input-001.zlib")
    , ("test/expected-002.txt", "test/input-002.zlib")
    , ("test/expected-003.bin", "test/input-003.zlib")
    ]

oneTest :: FilePath -> FilePath -> IO ()
oneTest expectedFile inputFile = do
  expected <- readFileBytes expectedFile
  input <- readFileBytes inputFile
  case Chunks.concat <$> decompress input of
    Left err -> putStrLn (show err) >> exitFailure
    Right actual | actual /= expected -> print actual >> exitFailure
    _ -> pure ()

readFileBytes :: FilePath -> IO Bytes
readFileBytes filepath = do
  contents <- BS.readFile filepath
  dst <- BA.newByteArray (BS.length contents)
  BS.unsafeUseAsCStringLen contents $ \(ptr, len) -> do
    let !(BA.MutableByteArray dst#) = dst
    Prim.copyPtrToMutablePrimArray (Prim.MutablePrimArray dst#) 0 ptr len
  byteArr <- BA.unsafeFreezeByteArray dst
  pure $ Bytes.fromByteArray byteArr
