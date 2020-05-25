# Byte Count Reader
-------------------

[![Hackage](https://img.shields.io/hackage/v/byte-count-reader.svg)][hackage]

This library is for reading strings describing a number of bytes like 2Kb and 0.5 MiB.

The units KB, MB, GB and TB imply base 10 (e.g. 2KB = 2 x 1000).
The units KiB, MiB, GiB and TiB imply base 2 (e.g. 2KiB = 2 * 1024).

The following are examples of strings that are accepted:
- 1b
- 2 KiB
- 3.5 MB
- 10 &nbsp;&nbsp; gib
- 100tb

[hackage]: http://hackage.haskell.org/package/github "Hackage"
