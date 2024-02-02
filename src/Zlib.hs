module Zlib
  ( decompress
  , ZlibError (..)
  ) where

import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks)
import Zlib.Raw (ZlibError (..), runZlib)

import qualified Zlib.Raw as Raw

decompress :: Bytes -> Either ZlibError Chunks
decompress inp = runZlib Raw.decompress inp
