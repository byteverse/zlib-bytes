#include "Rts.h"
#include "zlib.h"

HsInt hs_initDecompress
  ( z_stream *st
  , char *src // source buffer of compressed bytes
  , HsInt src_len // number of bytes available to be read from src
  ) {
  st->zalloc = Z_NULL;
  st->zfree = Z_NULL;
  st->opaque = Z_NULL;
  st->next_in = src;
  st->avail_in = (int)src_len;
  return (HsInt)(inflateInit(st));
}

HsInt hs_decompressChunk
  ( z_stream *st
  , char *dst // output buffer for decompressed bytes
  , HsInt dst_len // number of bytes available to write into dst
  ) {
  st->avail_out = (int)dst_len;
  st->next_out = dst;
  return (HsInt)(inflate(st, Z_NO_FLUSH));
}

HsInt hs_avail_out(z_stream *st) { return (HsInt)(st->avail_out); }
