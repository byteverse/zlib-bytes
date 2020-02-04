# zlib-bytes

Bindings to `zlib`. If something seems wrong with these bindings, try
using the CLI client with:

    cabal build --write-ghc-environment-files=always
    ghc example/decompress.hs
    example/decompress </path/to/compressed.bin >/path/to/decompressed.bin
