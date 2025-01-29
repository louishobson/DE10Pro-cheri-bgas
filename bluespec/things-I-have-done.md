# Things I have done

## Improving Toooba prefetching:

- Prefetches request shared access. But often prefetches miss in the L1D because the core wants exclusive access. A modern processor would do something preemptive here (reference) but this is treated as a normal miss by Toooba. Make the LL upgrade to exclusive access on all requests where the cache line isn't duplicated in another cache (i.e. all requests since we only have one core). Would be nice to implement speculative upgrade...

- When prefetches are late (or prefetch under prefetch), the prefetch previously took up an MSHR slot until dependency resolution. This isn't necesssary: just drop the prefetch and leave the slot open.

- wasPrefetch bit is implemented strangely: wiped on access: this is probably fine tho for his uses

- Increase the number of MSHRs to be more representative of modern cores (reference).