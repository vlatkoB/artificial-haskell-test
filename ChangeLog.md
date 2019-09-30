# Changelog for artificial-haskell-test


## 0.0.0.5
  Speed improvements (38 ms -> 35 ms):
  - Rename 'pointCoordInSpiral to 'seqTocoordspiral'
  - Parsed scroll is list of Word8
  - Direct conversion from coordinate to sequential
  - Introduced mutable Vector approach
  - `--no-template` calculates population on the fly


## 0.0.0.4
  Speed improvements (112 ms -> 38 ms):
  - optimise last digit check
  - pure sequential parsing
  - skip reverse, calculation with reversed world
  - simpler pattern match for `pointCoordInSpiral`
  - better error message
  - no length check
  - no chunking
  - parallel find of village coordinates

## 0.0.0.3
  Speed improvements (234 ms -> 112 ms):
  - Map -> HashMap
  - skip converting ByteString <-> String
  - do not use BC.cons - expensive
  - remove sequential parse
  - configurable chunk size for concurrent parse
  - optimise direct coordination find to have similar speed regardless of search position

## 0.0.0.2
  - Fix mempry leak and CPU choke
  - Add direct village coordinate search
  - Cmd-line options for specific processing type
  - Introduce few datatypes
  - Concurrent scroll parsing
