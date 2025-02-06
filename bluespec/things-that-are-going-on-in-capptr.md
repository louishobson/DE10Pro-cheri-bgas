- At 433185, the instruction for a floating point store to 0xc00308e0 retires
- At 433313, the actual store happens
- At 433314, we get the update ram log line that shows that the store actually happened

- There is a load triggered at 433176 for 0xc00308e0+4, which obviously gets the wrong data because it's before the store has happened. This load also misses.
- However, the instruction doesn't retire yet

- At 433315, the same load is triggered again, except this time it gets the right data
- At 433320, the load instruction retires

- Conclusion: the CPU is performing a load before the store has happened, realising, and then trying again later, after the store has happened
The prefetcher is then training itself on a speculative load of capabilities that just happen to be in memory at that point.

- learning the stack
- learning speculation