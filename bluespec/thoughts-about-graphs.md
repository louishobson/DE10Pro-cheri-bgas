1. Demand hit rate largely very high
    - Might be worth decreasing cache size further

2. Speedups for stride, but Karlis' prefetchers don't have much effect.
    - CHERI stride seems to be doing well...
    - CHERI stride_adaptive would probably be good, but probably not very insightful.
    - n.b. stride_adaptive has 5 steady states, when the stride is broken in the last two, jump back to steady1 instead of going to transient

3. Karlis' prefetchers really aren't doing much

    - all_in_cap is prefetching quite a bit.
        - Has very high hit rate in adpcm and dijkstra
        - Has very low accuracy in general (except for blowfish, where it is very high!)
        - Could learn what capability sizes this is good for?
        - This is esentially just the spatial prefetcher but for small capabilities!
        - Or in what PCs this works well?

    - Karlis has filters in the others: maybe they are too aggressive?
        - Just filter out using the L1D?

    - cap_spatial is doing some prefetches in aes, fft, and picojpeg
        - aes
            - small prefetch miss rate
            - very accurate for those that miss
            - quite a few late prefetches...
        - fft
            - reasonably high prefetch miss rate
            - 100% accuracy
            - negative coverage
                - but it's not evicting many in-use cache lines (according to my definition of in-use)...
        - picojpeg
            - reasonable prefetch miss rate
            - low accuracy (along with all other prefetchers)
            - negative coverage
                - lots of useless disruption
            - the only prefetcher not to have a negative effect (+cap_spatial_ptr)
        - note that the max cap size was 32768-bytes
            - this is really quite big
            - I would find it surprising that the most common regions in such a large capability would be accessed temporally close together
            - I think that a small working set that fits in the cache is giving us high accuracy

    - cap_ptr is doing very little
        - only really in basicmath, where most prefetches hit the cache
        - the others appear to be accurate tho.
        - maybe this is a sign to decrease the cache size?
        - looking at the number of new lines in the cache that have capabilities, feels like it could work well
        - the max capability size is _massive_.
            - It could include a large array of pointers: I don't think that useful trends will be visible there

    - cap_spatial_ptr... is weird
        - I would expect the number of prefetches to be the sum of cap_spatial and cap_ptr, but this isn't the case
        - Has a much larger spatial prefetcher (table size, max capability size)
            - but that should let it prefetch more!
            - should probably rerun with consistent parameters arrrrg (can do this with a shrunk L1D cache tho)

4. Overall thoughts

    - Lots of the cheri prefetchers aren't doing much. Possible reasons:
        1. The filters are too aggressive: I'll look into this
        2. More likely, the prefetchers just aren't gaining confidence in trends.
            - Can probably add some print statements inside the prefetchers to confirm this
            - I think they could work well on _much_ smaller capability sizes
            - Maybe it needs a better confidence system? Could understand this from the print statements.
        3. Just a thought: what are his replacement policies in the tables? Could we evict poorly performing entries?
        4. Another thought: could we do something with _really_ large capabilities based on their base address.
            - There are probably only small number of such really large capabilities, and they are probably accessed many times.
                - I have the capacity right now to determine this...


    