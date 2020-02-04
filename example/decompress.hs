import Zlib (decompress) 
import System.IO (stdin,stdout)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks

main :: IO ()
main = do
  everything <- Chunks.hGetContents stdin
  case decompress (Chunks.concat everything) of
    Left _ -> fail "could not decompress"
    Right r -> Bytes.hPut stdout (Chunks.concat r)
