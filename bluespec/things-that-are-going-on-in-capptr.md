Speculative loads are causing loads in basicmath:

- At 433185, the instruction for a floating point store to 0xc00308e0 retires
- At 433313, the actual store happens
- At 433314, we get the update ram log line that shows that the store actually happened

- There is a load triggered at 433176 for 0xc00308e0+4, which obviously gets the wrong data because it's before the store has happened. This load also misses.
- However, the instruction doesn't retire yet

- At 433315, the same load is triggered again, except this time it gets the right data
- At 433320, the load instruction retires

- The CPU is performing a load before the store has happened, realising, and then trying again later, after the store has happened. The prefetcher is then training itself on a speculative load of capabilities that just happen to be in memory at that point.

Things we can improve on:
- We load a capability that points onto the stack
    - If it is the csp pointer itself, then it is too big for the prefetcher to train itself on. Also, then its base is 0 (on Toooba), so any load from csp would trigger leaning
- Learning from speculative loads
    - Sometimes the triggering load is for < 16-byte region, which we know cannot contain a capability
    - Otherwise we could delay training until the load is no longer speculative
- The fetched pointer is often stored to, sometimes only stored to
    - Learn from stores as well as loads

Comments on improvements:
- After ensuring capabilities are at least 16 bytes, then no prefetches are issued for any benchmark.
- For basicmath, this appears to be because almost all accesses are via massive capabilities
- For dijkstra, the source code looks promising since there are nodes with pointers
    - But all of these are allocated on an array on the stack
    - This means they are accessed by one big capability, which is too big to be tracked (confirmed in log)
    - Even if we could track it, the offset being accessed will keep changing.
    - Maybe we could create a history of access offsets.
        - We could make like a delta prefetcher that can separate by capability base address
    - Use the set bounds built in!
- Qsort suffers from the same problem:
    - We see a size-16 capability load a capability pointing into the large array we are sorting
- None of the benchmarks except patricia use on-heap pointer chasing. 
    - I think it's pretty uncommon to see such patterns