import System.IO (stdin, stdout)
import Zlib (decompress)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks

main :: IO ()
main = do
  everything <- Chunks.hGetContents stdin
  case decompress (Chunks.concat everything) of
    Left err -> fail ("could not decompress: " ++ show err)
    Right r -> Bytes.hPut stdout (Chunks.concat r)
