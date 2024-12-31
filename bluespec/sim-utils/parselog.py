from collections import Counter, deque
from functools import partial
from typing import Iterable, Iterator, List, Tuple
import gc
import gzip
import matplotlib.pyplot as plt
import numpy as np
import re
import statistics



class LogLine:

    _TEST_REGEX = ""
    _DATA_REGEX = ""

    _TEST_REGEX_COMPILED = re.compile(_TEST_REGEX)
    _DATA_REGEX_COMPILED = re.compile(_DATA_REGEX)

    subLineTypes: List[type["LogLine"]] = []

    @classmethod
    def createSubLineType(cls, LineType):
        assert(issubclass(LineType, cls))
        LineType._TEST_REGEX_COMPILED = re.compile(LineType._TEST_REGEX)
        LineType._DATA_REGEX_COMPILED = re.compile(LineType._DATA_REGEX)
        LineType.subLineTypes = []
        cls.subLineTypes.append(LineType)
        return LineType
    
    @classmethod
    def testRegex(cls, text: str) -> bool:
        return cls._TEST_REGEX_COMPILED.match(text) is not None
        
    @classmethod
    def dataRegex(cls, text: str) -> Tuple[str, ...]:
        m = cls._DATA_REGEX_COMPILED.match(text)
        if m is None:
            raise ValueError(f"{cls.__name__}.dataRegex match failed!")
        return m.groups()
        
    @classmethod
    def deduceLineType(cls, line: str) -> type["LogLine"] | None:
        if not cls.testRegex(line):
            return None
        matchingSubLineTypes = {LineType.deduceLineType(line) for LineType in cls.subLineTypes}
        matchingSubLineTypes.discard(None)
        if len(matchingSubLineTypes) == 0:
            return cls
        elif len(matchingSubLineTypes) == 1:
            return matchingSubLineTypes.pop()
        else:
            raise ValueError("LogLine.deduceLineType: multiple line types match: ", matchingSubLineTypes)
        
    def __init__(self, line: str) -> None:
        self.line = None
        self.discard = False
        self.warning = False
        self.discardReason = None
        self.warningReason = None

    def discardIf(self, pred: bool, reason: str = "NoReason") -> bool:
        if pred:
            self.discard = True
            self.discardReason = reason
        return pred
    
    def warnIf(self, pred: bool, reason: str = "NoReason") -> bool:
        if pred:
            self.warning = True
            self.warningReason = reason
        return pred

    def postProcess(self, before: Iterable["LogLine"], after: Iterable["LogLine"]) -> None:
        pass

    def getTotals(self) -> dict[str, int]:
        rt = {
            "total":   int(not self.discard),
            "discard": int(self.discard),
            "warning": int(self.warning),
        }
        if self.discard:
            rt[self.discardReason] = 1
        if self.warning:
            rt[self.warningReason] = 1
        return rt

    def getDistributions(self) -> dict[str, int]:
        return {}



@LogLine.createSubLineType
class TimestampedLine(LogLine):

    _TEST_REGEX = r"^\d+"
    _DATA_REGEX = r"^(\d+)"

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = TimestampedLine.dataRegex(line)
        self.timestamp = int(reData[0])

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return
    
        # Check that timestamps are monotonically increasing.
        # A pretty fundamental problem if this is not the case, so raise an error.
        for ll in reversed(before):
            if isinstance(ll, TimestampedLine):
                if ll.timestamp > self.timestamp:
                    raise ValueError(f"TimestampedLine.postProcess: timestamps are not monotonically increasing: {ll.timestamp} -> {self.timestamp}")
                break
        for ll in after:
            if isinstance(ll, TimestampedLine):
                if ll.timestamp < self.timestamp:
                    raise ValueError(f"TimestampedLine.postProcess: timestamps are not monotonically increasing: {self.timestamp} -> {ll.timestamp}")
                break



@TimestampedLine.createSubLineType
class RVFILine(TimestampedLine):
    
    _TEST_REGEX = r"^\d+: RVFI Order"
    _DATA_REGEX = r"^\d+: RVFI Order: \s*(\d+), PC: (0x[0-9a-f]+), I: (0x[0-9a-f]+), PCWD: (0x[0-9a-f]+), Trap: ([01])"

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = RVFILine.dataRegex(line)
        self.rvfi      = int(reData[0])
        self.pc        = int(reData[1], 0)
        self.instr     = int(reData[2], 0)
        self.pcwd      = int(reData[3], 0)
        self.trap      = bool(reData[4] == "1")

    def getDistributions(self):
        return {} if self.discard else {
            "pc": self.pc
        }



@TimestampedLine.createSubLineType
class NonRVFILine(TimestampedLine):

    _TEST_REGEX = r"^(?!\d+: RVFI Order)"

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        # Find the previous RVFI by searching backwards
        # Failing that, search forwards
        self.prev_rvfi = None
        for ll in reversed(before):
            if isinstance(ll, RVFILine):
                self.prev_rvfi = ll.rvfi
                return
        for ll in after:
            if isinstance(ll, RVFILine):
                self.prev_rvfi = ll.rvfi - 1
                return
        self.discardIf(self.prev_rvfi is None, "NoPrevRVFI")

        

@NonRVFILine.createSubLineType
class CRqHitLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ L1D cRq hit"
    _DATA_REGEX = r"^\d+ L1D cRq hit: mshr: (\d+), addr: (0x[0-9a-f]+), cRq is prefetch: ([01]), wasMiss: ([01]), cs: ([ITSEM]), op: (Ld|St|Lr|Sc|Amo)"

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = CRqHitLine.dataRegex(line)
        self.mshr          = int(reData[0])
        self.addr          = int(reData[1], 0)
        self.cRqIsPrefetch = bool(reData[2] == "1")
        self.wasMiss       = bool(reData[3] == "1")
        self.cs            = str(reData[4])
        self.op            = str(reData[5])

        # Will be set by a prior PrefetchCreationLine if this hit is due to a prefetch
        self.prefetchCreationLine = None

        # Will be set by a prior CRqEvictionLine if this hit was once a miss
        self.cRqMissEvictionLine = None

    def postProcess(self, before, after):
        super().postProcess(before, after)
        if self.discard: return

    def getDistributions(self):
        return {} if self.discard or self.cRqIsPrefetch else {
            "addr" : self.addr << 6
        }



@NonRVFILine.createSubLineType
class CRqEvictionLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ L1D cRq line eviction"
    _DATA_REGEX = r"^\d+ L1D cRq line eviction \(([\w ]+)\): mshr: (\d+), old addr: (0x[0-9a-f]+), new addr: (0x[0-9a-f]+), wasPrefetch: ([01]), accessed: ([01]), cRq is prefetch: ([01]), cs: ([ITSEM]), op: (Ld|St|Lr|Sc|Amo)"

    # How many cycles after a cRq miss to expect the corresponding cache refill
    MAX_PRS_CYCLES = 40

    # How many cycles into the future to look for cache disruption
    MAX_CACHE_DISRUPTION_CYCLES = 1000

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = CRqEvictionLine.dataRegex(line)
        self.repType       = str(reData[0])
        self.mshr          = int(reData[1])
        self.oldAddr       = int(reData[2], 0)
        self.newAddr       = int(reData[3], 0)
        self.wasPrefetch   = bool(reData[4] == "1")
        self.accessed      = bool(reData[5] == "1")
        self.cRqIsPrefetch = bool(reData[6] == "1")
        self.cs            = str(reData[7])
        self.op            = str(reData[8])

        # Will be set by a prior PrefetchCreationLine if this eviction is due to a prefetch
        self.prefetchCreationLine = None

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        # If this cRq is due to a prefetch and we haven't been told about the prefetch creation, produce a warning
        self.warnIf(self.cRqIsPrefetch and self.prefetchCreationLine is None, "NoCreationOfPrefetchCRq")

        # If we just evicted a prefetched, unaccessed line, find the prefetch that caused it and mark it as useless
        # We might not find the prefetch if we skipped the start of the log, so only warn
        if self.wasPrefetch and not self.accessed and self.cs != "I":
            foundPrefetchCreation = False
            for ll in reversed(before):
                if isinstance(ll, CRqEvictionLine) and ll.newAddr == self.oldAddr:
                    # We expect the log line to be a prefetch
                    if self.discardIf(not ll.cRqIsPrefetch, "PriorEvictionNotPrefetch"): return
                    if ll.prefetchCreationLine is not None:
                        ll.prefetchCreationLine.isUselessPrefetch = True
                        foundPrefetchCreation = True
                    break
            self.warnIf(not foundPrefetchCreation, "NoCreationOfEvictedPrefetchedLine")

        # Look for the cache refill. When a pRs is received for the refill data, the cRqHit rule fires.
        # If this cRq is for a prefetch, also see if this prefetch was late, or whether there is a prefetch-under-prefetch.
        foundRefill = False
        self.refillCycles = None
        self.cRqRefillHitLine = None
        for ll in after:
            if(isinstance(ll, TimestampedLine)):
                if ll.timestamp > self.timestamp + self.MAX_PRS_CYCLES:
                    break
                # The refill for the miss
                if isinstance(ll, CRqHitLine) and ll.mshr == self.mshr:
                    if self.discardIf(ll.addr != self.newAddr or not ll.wasMiss, "StrangeRefill"): return
                    foundRefill = True
                    self.refillCycles = ll.timestamp - self.timestamp
                    ll.cRqMissEvictionLine = self
                    self.cRqRefillHitLine = ll
                    break
                # This is a prefetch and another prefetch occurred for the same address during refill.
                # Tell that prefetch that it's a duplicate.
                if isinstance(ll, PrefetchCreationLine) and self.cRqIsPrefetch and ll.addr == self.newAddr:
                    ll.isPrefetchUnderPrefetch = True
                # Look for a demand miss for the address currently being refilled.
                # If the current refill is due to a prefetch, then consider the prefetch as late.
                if isinstance(ll, CRqDependencyLine) and self.cRqIsPrefetch and ll.addr == self.newAddr and not ll.cRqIsPrefetch:
                    # It's possible that, if some log was skipped, the cRq is a prefetch but the creation was missed.
                    if self.prefetchCreationLine is not None and not self.prefetchCreationLine.isLatePrefetch:
                        self.prefetchCreationLine.isLatePrefetch = True
                        self.prefetchCreationLine.lateRelativeCycles = ll.timestamp - self.prefetchCreationLine.timestamp

        # If we didn't find a refill, discard this log line.
        # If due to a prefetch, also discard that prefetch.
        if self.discardIf(not foundRefill, "noPRsDiscard"):
            if self.prefetchCreationLine is not None:
                self.prefetchCreationLine.discardIf(True, "noPRsDiscard")
            return

        # Look for cache disruption.
        # This means that the evicted line missed in the recent future.
        self.disruptedCache = False
        self.disruptionCycles = None
        if self.cs != "I":
            for ll in after:
                if(isinstance(ll, TimestampedLine)):
                    if ll.timestamp > self.timestamp + self.MAX_CACHE_DISRUPTION_CYCLES:
                        break
                    if isinstance(ll, CRqEvictionLine) and ll.newAddr == self.oldAddr:
                        self.disruptedCache = True
                        self.disruptionCycles = ll.timestamp - self.timestamp
                        break

    def getTotals(self) -> dict[str, int]:
        rt = super().getTotals()
        return rt if self.discard else rt | {
            "wasPrefetch"        : int(self.wasPrefetch),
            "accessed"           : int(self.accessed),
            "unaccessedPrefetch" : int(self.wasPrefetch and self.accessed),
            "disruption"         : int(self.disruptedCache),
        }
    
    def getDistributions(self) -> dict[str, int]:
        if self.discard:
            return {}
        rt = {"refillCycles" : self.refillCycles}
        if self.disruptedCache:
            rt["disruptionCycles"] = self.disruptionCycles
        return rt



@NonRVFILine.createSubLineType
class CRqDependencyLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ L1D cRq dependency"
    _DATA_REGEX = r"^\d+ L1D cRq dependency: mshr: (\d+), depMshr: (\d+), addr: (0x[0-9a-f]+), cRq is prefetch: ([01]), op: (Ld|St|Lr|Sc|Amo)"

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = CRqDependencyLine.dataRegex(line)
        self.mshr          = int(reData[0])
        self.depMshr       = int(reData[1])
        self.addr          = int(reData[2], 0)
        self.cRqIsPrefetch = bool(reData[3] == "1")
        self.op            = str(reData[4])

        # Will be set by a prior PrefetchCreationLine if line is due to a prefetch
        self.prefetchCreationLine = None

    def postProcess(self, before, after):
        super().postProcess(before, after)
        if self.discard: return

        # If this cRq is due to a prefetch and we haven't been told about the prefetch creation, produce a warning
        self.warnIf(self.cRqIsPrefetch and self.prefetchCreationLine is None, "NoCreationOfPrefetchCRq")



@NonRVFILine.createSubLineType
class PRqEvictionLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ L1D pRq line eviction"
    _DATA_REGEX = r"^\d+ L1D pRq line eviction: addr: (0x[0-9a-f]+), wasPrefetch: ([01]), accessed: ([01]), cs: ([ITSEM])"

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = PRqEvictionLine.dataRegex(line)
        self.addr        = int(reData[0], 0)
        self.wasPrefetch = bool(reData[1] == "1")
        self.accessed    = bool(reData[2] == "1")
        self.cs          = str(reData[3])

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return
        
        # If we just evicted a prefetched, unaccessed line, find the prefetch that caused it and mark it as useless
        # We might not find the prefetch if we skipped the start of the log
        if self.wasPrefetch and not self.accessed and self.cs != "I":
            foundPrefetchCreation = False
            for ll in reversed(before):
                if isinstance(ll, CRqEvictionLine) and ll.newAddr == self.addr:
                    # We expect the log line to be a prefetch
                    if self.discardIf(not ll.cRqIsPrefetch, "PriorEvictionNotPrefetch"): return
                    if ll.prefetchCreationLine is not None:
                        ll.prefetchCreationLine.isUselessPrefetch = True
                        foundPrefetchCreation = True
                    break
            self.warnIf(not foundPrefetchCreation, "NoCreationOfEvictedPrefetchedLine")

    def getTotals(self):
        rt = super().getTotals()
        return rt if self.discard else rt | {
            "wasPrefetch"        : int(self.wasPrefetch),
            "accessed"           : int(self.accessed),
            "unaccessedPrefetch" : int(self.wasPrefetch and self.accessed),
        }



@NonRVFILine.createSubLineType
class PrefetchCreationLine(NonRVFILine):
    
    _TEST_REGEX = r"^\d+ L1D createPrefetchRq"
    _DATA_REGEX = r"^\d+ L1D createPrefetchRq: mshr: (\d+), addr: (0x[0-9a-f]+), mshrInUse: \s*(\d+)/\s*(\d+)"

    # How many cycles in the future to expect a cRq response for the prefetch
    MAX_CRQ_RESP_CYCLES = 30

    # How many cycles before the prefetch was even issued to look for a miss for this address
    MAX_SUPER_LATE_CYCLES = 500

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = PrefetchCreationLine.dataRegex(line)
        self.mshr      = int(reData[0])
        self.addr      = int(reData[1], 0)
        self.mshrUsed  = int(reData[2])
        self.totalMshr = int(reData[3])
        # This attribute may be set by other PrefetchCreationLine's in post processing
        self.isPrefetchUnderPrefetch = False
        # This attribute is set by CRqEvictionLine for an evicted, unused prefetch
        self.isUselessPrefetch = False

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        # Look whether the prefetch was super late
        # That is, there was a miss for this address before the prefetch was even suggested
        # Also record the relative timestamp of the associated miss
        # Will be negative for a super late prefetch
        self.isLatePrefetch = False
        self.lateRelativeCycles = None
        for ll in reversed(before):
            if isinstance(ll, TimestampedLine):
                if ll.timestamp + self.MAX_SUPER_LATE_CYCLES < self.timestamp:
                    break
                # There is an earlier demand miss for the same address
                if isinstance(ll, CRqEvictionLine) and ll.newAddr == self.addr and not ll.cRqIsPrefetch:
                    self.isLatePrefetch = True
                    self.lateRelativeCycles = ll.timestamp - self.timestamp
                    break
                # There is an earlier prefetch for the same address.
                # In this case, don't consider this prefetch as being late (rather this prefetch is probably a duplicate).
                if isinstance(ll, PrefetchCreationLine) and ll.addr == self.addr:
                    break

        # The prefetch first checks the cache for whether it is necessary via a cRq.
        # Look for the cRq exiting the pipeline.
        # We will then know whether the prefetch hit or miss.
        # Also keep looking for whether this prefetch is late.
        self.prefetchHit = False
        self.prefetchMiss = False
        self.prefetchOwned = False
        self.prefetchCRqCycles = None
        self.prefetchResponseLine = None
        for ll in after:
            if isinstance(ll, TimestampedLine):
                if ll.timestamp > self.timestamp + self.MAX_CRQ_RESP_CYCLES:
                    break
                # Prefetch address hit in the cache
                if isinstance(ll, CRqHitLine) and ll.mshr == self.mshr:
                    if self.discardIf(ll.addr != self.addr or not ll.cRqIsPrefetch, "StrangePrefetchResponse(hit)"): return                 
                    self.prefetchHit = True
                    self.prefetchCRqCycles = ll.timestamp - self.timestamp
                    self.prefetchResponseLine = ll
                    ll.prefetchCreationLine = self
                    break
                # Prefetch address missed in the cache
                if isinstance(ll, CRqEvictionLine) and ll.mshr == self.mshr:
                    if self.discardIf(ll.newAddr != self.addr or not ll.cRqIsPrefetch, "StrangePrefetchResponse(miss)"): return           
                    self.prefetchMiss = True
                    self.prefetchCRqCycles = ll.timestamp - self.timestamp
                    self.prefetchResponseLine = ll
                    ll.prefetchCreationLine = self
                    break
                # Cache line for the prefetch address is owned (maybe late or duplicate prefetch)
                if isinstance(ll, CRqDependencyLine) and ll.mshr == self.mshr:
                    if self.discardIf(ll.addr != self.addr or not ll.cRqIsPrefetch, "StrangePrefetchResponse(owned)"): return           
                    self.prefetchOwned = True
                    self.prefetchCRqCycles = ll.timestamp - self.timestamp
                    self.prefetchResponseLine = ll
                    ll.prefetchCreationLine = self
                    break
                # Another prefetch occurred for the same address.
                # Tell that prefetch that it's a duplicate.
                if isinstance(ll, PrefetchCreationLine) and ll.addr == self.addr:
                    ll.isPrefetchUnderPrefetch = True
                # This prefetch is late: there is a demand cache miss for the same address in a different MSHR
                # This is quite unlikely: needs the core to request the address a cycle before prefetch creation (I think....)
                if isinstance(ll, CRqEvictionLine) and ll.mshr != self.mshr and ll.newAddr == self.addr and not ll.cRqIsPrefetch and not self.isLatePrefetch:
                    self.isLatePrefetch = True
                    self.lateRelativeCycles = ll.timestamp - self.timestamp

        # If we didn't find a response, discard this line
        if self.discardIf(not self.prefetchHit and not self.prefetchMiss and not self.prefetchOwned, "noCRqDiscard"):
            return
        
        # If the prefetch missed, then post processing for the CRqEvictionLine may still deduce
        # that this prefetch was late.       


    
    def getTotals(self) -> dict[str, int]:
        rt = super().getTotals()
        return rt if self.discard else rt | {
            "hit"               : int(self.prefetchHit),
            "miss"              : int(self.prefetchMiss),
            "owned"             : int(self.prefetchOwned),
            "late"              : int(self.isLatePrefetch),
            "useless"           : int(self.isUselessPrefetch),
            "prefUnderPref"     : int(self.isPrefetchUnderPrefetch),
            "uselessDisruption" : int(self.isUselessPrefetch and self.prefetchResponseLine.disruptedCache),
        }
        
    def getDistributions(self) -> dict[str, int]:
        if self.discard:
            return {}
        rt = {
            "cRqCycles" : self.prefetchCRqCycles,
            "mshrRemaining" : self.totalMshr - self.mshrUsed
        }
        if self.isLatePrefetch:
            rt["lateRelativeCycles"] = self.lateRelativeCycles
        return rt



class LogParser:

    class openMaybeGZip:
        def __init__(self, filename: str):
            self.filename = filename
        def __enter__(self):
            if self.filename.endswith(".gz") or self.filename.endswith(".gzip"):
                self.fp = gzip.open(self.filename, "rb")
            else:
                self.fp = open(self.filename, "rb")
            return self.fp
        def __exit__(self, *args):
            self.fp.close()

    @staticmethod
    def niceReadLines(fp, chunksize=512*1024*1024) -> Iterable[str]:
        gc.disable()
        oldChunk = ""
        while (chunk := fp.read(chunksize).decode("utf-8")):
            lines = (oldChunk + chunk).split('\n')
            for line in lines[:-1]:
                yield line
            oldChunk = lines[-1]
            del lines
            del chunk
            gc.collect()
        gc.enable()

    def __init__(
        self, 
        log: str, 
        linesToSkip: int | None = None,
        maxLines: int | None = None, 
        lineTypesToPrune: List[type[LogLine]] = [],
        lineTypesToError: List[type[LogLine]] = [],
        RootLogLine: type[LogLine] = LogLine
    ) -> None:

        self.logLines: deque[LogLine] = deque()
        self.lineTypeCounts: dict[type[LogLine], int] = {}

        # Load relevant log lines into self.logLines
        # Skip any line types in lineTypesToPrune
        # Error if any lines in lineTypesToError are found
        with LogParser.openMaybeGZip(log) as fp:
            for line in self.niceReadLines(fp):
                # Remove whitespace from start end end of the log line before decucing its type
                line = line.strip()
                LineType = RootLogLine.deduceLineType(line)
                # Check lineTypesToPrune (continue on match)
                if LineType in lineTypesToPrune:
                    continue
                # Check lineTypesToError
                if LineType is None or LineType in lineTypesToError:
                    raise ValueError(f"Found LineType {LineType.__name__}, which is either None or in the error list")
                # Skip the line if the user specified linesToSkip and there are lines remaining to skip
                if linesToSkip is not None and linesToSkip > 0:
                    linesToSkip -= 1
                    continue
                # Instantiate the line type and save it to logLines
                self.logLines.append(LineType(line))
                self.lineTypeCounts[LineType] = self.lineTypeCounts.get(LineType, 0) + 1
                # Check we haven't recorded maxLines log lines
                if maxLines is not None and len(self.logLines) >= maxLines:
                    break
                # Print a status update
                if len(self.logLines) % 10000 == 0:
                    print(f"\rLoaded {len(self.logLines)} log lines", end="")

        # Final line counts
        print(f"\rLoaded {len(self.logLines)} log lines")
        maxLineNameLength = max(len(LineType.__name__) for LineType in self.lineTypeCounts)
        for LineType, count in self.lineTypeCounts.items():
            print(f"\t{(LineType.__name__+':').ljust(maxLineNameLength+1)} {count} instances")

        # Post-process lines
        before = deque()
        after  = self.logLines.copy()
        while len(after) != 0:
            current = after.popleft()
            current.postProcess(before, after)
            before.append(current)
            if(len(before) % 10000 == 0):
                print(f"\rPost-processed {len(before)} log lines", end="")
        del before
        del after
        print(f"\rPost-processed {len(self.logLines)} log lines")

        # Calculate totals and distributions
        self.totals = {}
        self.dists  = {}
        for i, ll in enumerate(self.logLines):
            totals = self.totals.setdefault(ll.__class__, {})
            dists  = self.dists.setdefault(ll.__class__, {})
            for k, v in ll.getTotals().items():
                totals[k] = totals.get(k, 0) + v
            for k, v in ll.getDistributions().items():
                if k not in dists:
                    dists[k] = []
                dists[k].append(v)
            if i % 10000 == 0:
                print(f"\rAccumulated totals and dists for {i} log lines", end="")
        print(f"\rAccumulated totals and dists for {len(self.logLines)} log lines")
            
        
    def printTotals(self) -> None:
        for LineType, totals in self.totals.items():
            if len(totals) != 0:
                print(f"{LineType.__name__} totals:")
                for k, v in totals.items():
                    print(f"\t{k}: {v}")
                print()

    def plotDists(self) -> List:
        figaxs = []
        for LineType, dists in self.dists.items():
            for dataName, data in dists.items():
                xsAreAddresses = min(data) >= 0xc0000000
                xsAddressGranuality = None
                if xsAreAddresses: # Assume addresses
                    xsAddressGranuality = min((x & -x).bit_length()-1 for x in data)
                    xsAddressGranuality = max(xsAddressGranuality, 2)
                    data = [x>>xsAddressGranuality for x in data]
                counts = Counter(data)
                xs = list(counts.keys())
                ys = list(counts.values())
                minxs = min(xs)
                maxxs = max(xs)
                xtickIntervalOpts  = [100,50,25,20,10,5,2,1] if not xsAreAddresses else [2**x for x in range(32,-1,-1)]
                xtickIdealCount    = 6 if not xsAreAddresses else 12
                xtickExactInterval = max(1, (maxxs-minxs)//xtickIdealCount)
                xtickNiceInterval  = next(i for i in xtickIntervalOpts if i <= xtickExactInterval)
                xtickMin = minxs-(minxs%xtickNiceInterval)
                xtickMax = maxxs+xtickNiceInterval
                fig, ax = plt.subplots(figsize=(8, 6))
                ax.bar(xs, ys, color='skyblue', width=1)
                ax.set_title(f"{LineType.__name__}: {dataName}")
                ax.set_xlabel("Attribute value")
                ax.set_ylabel("Count")
                if xsAreAddresses:
                    ysAvg   = int(statistics.mean(ys))
                    ysStdev = int(statistics.stdev(ys))
                    ax.set_ylim(0,ysAvg+3*ysStdev)
                    ax.set_xticks(
                        range(xtickMin, xtickMax, xtickNiceInterval),
                        [hex(x << xsAddressGranuality) for x in range(xtickMin, xtickMax, xtickNiceInterval)],
                        rotation=90
                    )
                else:
                    ax.set_xticks(range(xtickMin, xtickMax, xtickNiceInterval))
                if minxs < 0 and maxxs > 0:
                    ax.axvline(x=0, linestyle="--", color="black", alpha=0.5)
                ax.grid(axis='y', alpha=0.75)
                figaxs.append((fig, ax))