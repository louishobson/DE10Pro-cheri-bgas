# Thoughts I have had

- Karlis commented that many accesses are on very large bounds. Can we use the bound base as a hash to consensus prefetch?

- Toooba thing: how out of order are loads? Can we give an rvfi ID to the L1D cache to help the prefetcher out in recognising strides?

- CapSpacial: does it really need the filter table? Just filter them out in the L1D cache

- CapSpacial: including alignment in the usage table feels problematic, is there a more efficient encoding we could use...?

- Both: if capabilities are massive, I don't think that looking for the most commonly accessed fields makes sense.
There is no way accesses will be timely. This only makes sense for quite small capabilities I think.

- Large capabilities: there probably aren't many of these and they are probably accessed many times.
Therefore record information about the specific capability (i.e. via the bounds base).

- CHERI good for learning "macro strides"

- Disable filter table? fairly cheap to check in the L1 cache.

- Pointer prefetcher triggering off cap size. This makes sense for structures, but I don't think it makes sense for:
    - Singular pointers
        - Contain a single capability, so will all be the same size!
        - The offset in the capability accessed will change for each use case
        - Maybe need to incorporate PC in this case
    - Loading pointers from the stack pointer
        - Will be the same size within a function
        - Could we use the stack pointer to create different contexts for the prefetcher??
            - Recognise different functions?

- Pointer prefetcher only triggering when the cache line contianing the pointer misses. Could be more aggressive here?