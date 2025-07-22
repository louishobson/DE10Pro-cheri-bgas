## Remappings
- dmem.evt_AMO -> TRAP
- dmem.evt_AMO_MISS -> JAL
- dmem.evt_AMO_MISS_LAT -> JALR

- rob full -> BRANCH

- llmem.evt_EVICT -> REDIRECT
- llmem.evt_TLB_FLUSH -> tgc.evt_EVICT
- llmem.evt_ST -> tgc.evt_WRITE

- llcache.evt_TLB -> tgc.evt_READ
- llcache.evt_TLB_MISS -> tgc.evt_READ_MISS



## L1-D Cache

- Useful prefetches: AMO -> TRAP
- Prefetches that missed the cache: AMO_MISS -> JAL
- Prefetches into the L1 (excluding forwarded prefetches): AMO_MISS_LAT -> JALR

## LL Cache

- Prefetches into the L2 (including forwarded prefetches): ST -> tgc.evt_WRITE
- Prefetches that missed the cache: EVICT -> REDIRECT
- Prefetches that were useful: TLB_FLUSH -> tgc.evt_EVICT

## LLC TLB

- Requests: TLB -> tgc.evt_READ
- Misses: TLB_MISS -> tgc.READ_MISS
