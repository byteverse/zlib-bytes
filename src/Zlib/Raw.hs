{-# language BangPatterns #-}
{-# language CApiFFI #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
{-# language MultiParamTypeClasses #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}
{-# language UnliftedFFITypes #-}
{-# language ViewPatterns #-}

module Zlib.Raw
  ( Zlib
  , runZlib
  , decompress
  , ZlibError(..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, lift)
import Control.Monad.Except (MonadError(throwError,catchError))
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.ST (runST)
import Control.Monad.ST (ST)
import Data.Bytes (Bytes)
import Data.Bytes.Chunks (Chunks(ChunksCons,ChunksNil))
import Data.Primitive.ByteArray (MutableByteArray(MutableByteArray))
import Data.Primitive.ByteArray (newByteArray, newPinnedByteArray)
import Data.Word (Word8)
import Foreign.C.Types (CInt(CInt))
import Foreign.Ptr (Ptr)
import GHC.Exts (MutableByteArray#,touch#)
import GHC.IO (IO(IO),unsafeIOToST)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Primitive.ByteArray as BA


-- FIXME there are kinda two monads: ZlibCompress, ZlibDecompress
-- so far, I've only done the latter
newtype Zlib s a = Zlib { unZlib :: ReaderT (Stream s) (ExceptT ZlibError (ST s)) a }
  deriving(Functor, Applicative, Monad)

instance MonadError ZlibError (Zlib s) where
  throwError exn = Zlib (throwError exn)
  catchError try handle = Zlib (unZlib try `catchError` (unZlib . handle))

newtype Stream s = Stream
  { unStream :: MutableByteArray s
  }

-- TODO: In GHC 8.10+, use with# instead of touch# so that the
-- noinline pragma is not needed.
runZlib :: (forall s. Zlib s a) -> Bytes -> Either ZlibError a
{-# noinline runZlib #-}
runZlib action inp = runST $ runExceptT $ do
  let pinnedInp = Bytes.pin inp
  stream <- newStream pinnedInp
  v <- runReaderT (unZlib action) stream `onException` (\exn -> delStream stream >> throwError exn)
  _ <- delStream stream
  Bytes.touch pinnedInp
  pure v


------------ Idiomatic FFI Calls ------------

type PreZlib s a = ExceptT ZlibError (ST s) a

-- Precondition: Bytes are pinned.
-- Postcondition: Call touch on the argument after calling this function.
newStream :: Bytes -> PreZlib s (Stream s)
newStream pinnedInp = do
  let inpP = Bytes.contents pinnedInp
      inpLen = Bytes.length pinnedInp
  MutableByteArray stream# <- newPinnedByteArray sizeofStream
  ret <- lift . unsafeIOToST $ initDecompress stream# inpP inpLen
  let stream = Stream
        { unStream = MutableByteArray stream#
        }
  case ret of
    Z_OK -> pure stream
    Z_MEM_ERROR -> errorWithoutStackTrace "zlib: out of memory"
    Z_VERSION_ERROR -> errorWithoutStackTrace "zlib: incompatible version"
    Z_STREAM_ERROR -> throwError InvalidInitParameters
    _ -> errorWithoutStackTrace ("unknown error produced by zlib: " ++ show ret)

delStream :: Stream s -> PreZlib s ()
delStream stream = do
  let !(MutableByteArray stream#) = unStream stream
  ret <- lift . unsafeIOToST $ inflateEnd stream#
  case ret of
    Z_OK -> pure ()
    Z_STREAM_ERROR -> throwError InvalidStreamState
    _ -> errorWithoutStackTrace ("unknown error produced by zlib: " ++ show ret)

-- TODO couldn't I resize the output buffer rather than use chunks?
-- probably more useful for an unsliced version
decompress :: Zlib s Chunks
decompress = Zlib $ loop ChunksNil
  where
  -- TODO adapt chunkSize based on input remaining and estimated compression ratio
  chunkSize = 32 * 1024 :: Int
  loop acc = do
    !(MutableByteArray stream#) <- asks unStream
    !oBuf@(MutableByteArray oBuf#) <- newPinnedByteArray chunkSize
    ret <- lift . lift . unsafeIOToST $ do
      r <- decompressChunk stream# oBuf# chunkSize
      -- This call to touch# is not really necessary since GHC cannot
      -- possibly have any insight into what ret is, but it is prudent
      -- to include it here anyway.
      touchMutableByteArray# oBuf#
      pure r
    case ret of
      Z_OK -> do
        out <- Bytes.fromByteArray <$> BA.unsafeFreezeByteArray oBuf
        let acc' = ChunksCons out acc
        loop acc'
      Z_STREAM_END -> do
        out <- Bytes.fromByteArray <$> BA.unsafeFreezeByteArray oBuf
        outRestCInt <- lift . lift . unsafeIOToST $ availOut stream#
        let outRest = fromIntegral @CInt @Int outRestCInt
        let outLen = chunkSize - outRest
        pure $ Chunks.reverse $ case outLen of
          0 -> acc
          _ -> ChunksCons (Bytes.unsafeTake outLen out) acc
      Z_NEED_DICT -> errorWithoutStackTrace "zlib: preset dictionary is needed to decompress"
      Z_DATA_ERROR -> throwError DataCorrupt
      Z_STREAM_ERROR -> throwError InvalidStreamState
      Z_MEM_ERROR -> errorWithoutStackTrace "zlib: out of memory"
      Z_BUF_ERROR -> throwError BufferTooSmall
      _ -> errorWithoutStackTrace ("unknown error produced by zlib: " ++ show ret)

touchMutableByteArray# :: MutableByteArray# s -> IO ()
touchMutableByteArray# x = IO (\s -> (# touch# x s, () #))

------------ Idiomatic Error Handling ------------

data ZlibError
  = InvalidInitParameters -- corresponds to Z_STREAM_ERROR
  | InvalidStreamState -- corresponds to Z_STREAM_ERROR
  | DataCorrupt -- corresponds to Z_DATA_ERROR
  | BufferTooSmall -- corresponds to Z_BUF_ERROR
  deriving (Show)

instance Exception ZlibError where


pattern Z_BUF_ERROR :: CInt
pattern Z_BUF_ERROR <- ((== z_BUF_ERROR) -> True)
  where Z_BUF_ERROR = z_BUF_ERROR

pattern Z_DATA_ERROR :: CInt
pattern Z_DATA_ERROR <- ((== z_DATA_ERROR) -> True)
  where Z_DATA_ERROR = z_DATA_ERROR

pattern Z_MEM_ERROR :: CInt
pattern Z_MEM_ERROR <- ((== z_MEM_ERROR) -> True)
  where Z_MEM_ERROR = z_MEM_ERROR

pattern Z_NEED_DICT :: CInt
pattern Z_NEED_DICT <- ((== z_NEED_DICT) -> True)
  where Z_NEED_DICT = z_NEED_DICT

pattern Z_OK :: CInt
pattern Z_OK <- ((== z_OK) -> True)
  where Z_OK = z_OK

pattern Z_STREAM_END :: CInt
pattern Z_STREAM_END <- ((== z_STREAM_END) -> True)
  where Z_STREAM_END = z_STREAM_END

pattern Z_STREAM_ERROR :: CInt
pattern Z_STREAM_ERROR <- ((== z_STREAM_ERROR) -> True)
  where Z_STREAM_ERROR = z_STREAM_ERROR

pattern Z_VERSION_ERROR :: CInt
pattern Z_VERSION_ERROR <- ((== z_VERSION_ERROR) -> True)
  where Z_VERSION_ERROR = z_VERSION_ERROR


------------ Raw Foreign Imports ------------

foreign import capi "zlib.h value Z_BUF_ERROR" z_BUF_ERROR :: CInt
foreign import capi "zlib.h value Z_DATA_ERROR" z_DATA_ERROR :: CInt
foreign import capi "zlib.h value Z_MEM_ERROR" z_MEM_ERROR :: CInt
foreign import capi "zlib.h value Z_NEED_DICT" z_NEED_DICT :: CInt
foreign import capi "zlib.h value Z_OK" z_OK :: CInt
foreign import capi "zlib.h value Z_STREAM_END" z_STREAM_END :: CInt
foreign import capi "zlib.h value Z_STREAM_ERROR" z_STREAM_ERROR :: CInt
foreign import capi "zlib.h value Z_VERSION_ERROR" z_VERSION_ERROR :: CInt

foreign import capi "hs_zlib.h value hs_sizeofStream" sizeofStream :: Int

foreign import ccall unsafe "hs_initDecompress" initDecompress ::
     MutableByteArray# s
  -> Ptr Word8
  -> Int
  -> IO CInt

foreign import ccall unsafe "hs_decompressChunk" decompressChunk ::
     MutableByteArray# s
  -> MutableByteArray# s
  -> Int
  -> IO CInt

foreign import ccall unsafe "hs_avail_out" availOut ::
     MutableByteArray# s
  -> IO CInt

foreign import ccall unsafe "inflateEnd" inflateEnd ::
     MutableByteArray# s
  -> IO CInt
