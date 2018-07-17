# lazy-random-bytestring

Repo for Stackoverflow question.

 `Main.hs` has all the code, `make prof` runs the profiler.

### Profiling results

| Variant               | Desc                                                         |         bytes |
| --------------------- | :----------------------------------------------------------- | ------------: |
| lazyRandomByteString1 | Recurse with `cons`                                          | 1,032,068,448 |
| lazyRandomByteString2 | Recurse with `foldr`                                         |   689,582,816 |
| lazyRandomByteString3 | Use [`Builder`](https://hackage.haskell.org/package/bytestring-0.10.0.2/docs/Data-ByteString-Lazy-Builder.html#t:Builder) |   745,111,152 |

