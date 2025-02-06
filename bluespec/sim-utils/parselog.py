from collections import Counter, deque
from functools import partial
from typing import Callable, Iterable, List, Tuple
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
            raise ValueError(
                f"{cls.__name__}.dataRegex match failed!"
                f" Tried to match\n\t'{text}'\n with\n\t'{cls._DATA_REGEX}'"
            )
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
        self.line = line
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

    def preProcess(self, before: Iterable["LogLine"], after: Iterable["LogLine"]) -> None:
        pass

    def postProcess(self, before: Iterable["LogLine"], after: Iterable["LogLine"]) -> None:
        pass

    def getTotals(self) -> dict[str, int]:
        rt = {
            "total"   : int(not self.discard),
            "discard" : int(self.discard),
            "warning" : int(self.warning),
        }
        if self.discard:
            rt[f"discard({self.discardReason})"] = 1
        if self.warning:
            rt[f"warning({self.warningReason})"] = 1
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

    def preProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().preProcess(before, after)
        
        # Fix any timestamps that appear to have used $time instead of cur_cycles
        for ll in reversed(before):
            if isinstance(ll, TimestampedLine):
                if self.timestamp >= ll.timestamp * 10 and self.timestamp % 10 == 0:
                    self.timestamp = self.timestamp // 10
                break

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        # Check that timestamps are monotonically increasing.
        # It is a pretty fundamental problem if this is not the case, so raise an error.
        for ll in reversed(before):
            if isinstance(ll, TimestampedLine):
                if ll.timestamp > self.timestamp:
                    raise ValueError(f"TimestampedLine.postProcess: timestamps are not monotonically increasing: {ll.timestamp} -> {self.timestamp}")
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
        self.prevRvfiLine = None
        self.nextRvfiLine = None

    def getDistributions(self):
        return {} if self.discard else {
            "pc": self.pc
        }
    
    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        for ll in after:
            if isinstance(ll, RVFILine):
                self.nextRvfiLine = ll
                ll.prevRvfiLine = self
                break



@TimestampedLine.createSubLineType
class NonRVFILine(TimestampedLine):

    _TEST_REGEX = r"^(?!\d+: RVFI Order)"

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        # Find the previous RVFI by searching backwards
        # Failing that, search forwards
        self.prevRvfiLine = None
        self.nextRvfiLine = None
        self.rvfi = None
        for ll in reversed(before):
            if isinstance(ll, RVFILine):
                self.prevRvfiLine = ll
                self.rvfi = ll.rvfi
                break
        for ll in after:
            if isinstance(ll, RVFILine):
                self.nextRvfiLine = ll
                if self.rvfi is None:
                    self.rvfi = ll.rvfi - 1
                break
        self.discardIf(self.rvfi is None, "no previous RVFI")

        

@NonRVFILine.createSubLineType
class CRqCreationLine(NonRVFILine):
    
    _TEST_REGEX = r"^\d+ L1D cRq creation"
    _DATA_REGEX = r"^\d+ L1D cRq creation: mshr: (\d+), addr: (0x[0-9a-f]+), boundsVirtBase: (0x[0-9a-f]+), boundsOffset: (0x[0-9a-f]+), boundsLength: (0x[0-9a-f]+), pcHash: (0x[0-9a-f]+), mshrInUse: \s*(\d+)/\s*(\d+), isPrefetch: ([01]), isRetry: ([01]), reqCs: ([ITSEM]), op: (Ld|St|Lr|Sc|Amo)"

    # How many cycles in the future to expect a cRq response
    MAX_CRQ_RESP_CYCLES = 30

    # How many cycles before a prefetch was even issued to look for a miss for this address
    MAX_LATE_PREFETCH_ISSUE_CYCLES = 20

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = CRqCreationLine.dataRegex(line)
        self.mshr         = int(reData[0])
        self.addr         = int(reData[1], 0)
        self.lineAddr     = self.addr >> 6
        self.boundsBase   = int(reData[2], 0)
        self.boundsOffset = int(reData[3], 0)
        self.boundsLength = int(reData[4], 0)
        self.pcHash       = int(reData[5], 0)
        self.mshrUsed     = int(reData[6])
        self.totalMshr    = int(reData[7])
        self.isPrefetch   = int(reData[8] == "1")
        self.isDemand     = not self.isPrefetch
        self.isRetry      = int(reData[9] == "1")
        self.reqCs        = str(reData[10])
        self.op           = str(reData[11])
        # Will be set in self.postProcess
        self.hit   = False
        self.miss  = False
        self.owned = False
        self.cRqResponseLine   = None
        self.cRqHitLine        = None
        # Will be set in self.postProcess if the creation is a prefetch
        self.isLatePrefetch = False
        self.latePrefetchRelativeCycles = None
        # This attribute may be set by other CRqCreationLine's in post processing
        self.isPrefetchUnderPrefetch = False
        # This attribute is set by CRqMissLine for an evicted, unused prefetch
        self.isNeverAccessed = False
        self.isNeverAccessedBecausePerms = False
        # Set by the eventual CRqHitLine
        self.disruptedCache   = False
        self.disruptionCycles = None

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        # Look for the cRq response coming out of the pipeline.
        # We will then know whether we had a cache hit or miss.
        # Also look for whether this prefetch is late.
        for ll in after:
            if isinstance(ll, TimestampedLine):
                if ll.timestamp > self.timestamp + self.MAX_CRQ_RESP_CYCLES:
                    break
                # Address hit in the cache
                if isinstance(ll, CRqHitLine) and ll.mshr == self.mshr:
                    if self.discardIf(ll.addr != self.addr or ll.cRqIsPrefetch != self.isPrefetch or ll.wasMiss, "strange cRq response (hit)"): return                 
                    self.hit = True
                    self.cRqResponseLine   = ll
                    ll.cRqCreationLine     = self
                    break
                # Address missed in the cache
                if isinstance(ll, CRqMissLine) and ll.mshr == self.mshr:
                    if self.discardIf(ll.addr != self.addr or ll.cRqIsPrefetch != self.isPrefetch, "strange cRq response (miss)"): return           
                    self.miss = True
                    self.cRqResponseLine   = ll
                    ll.cRqCreationLine     = self
                    break
                # Cache line for the address is owned
                if isinstance(ll, CRqDependencyLine) and ll.mshr == self.mshr:
                    if self.discardIf(ll.addr != self.addr or ll.cRqIsPrefetch != self.isPrefetch, "strange cRq response (owned)"): return           
                    self.owned = True
                    self.cRqResponseLine   = ll
                    ll.cRqCreationLine     = self
                    break
                # This is a prefetch and another prefetch occurred for the same address.
                # Tell that prefetch that it's a duplicate.
                if isinstance(ll, CRqCreationLine) and self.isPrefetch and ll.isPrefetch and ll.lineAddr == self.lineAddr:
                    ll.isPrefetchUnderPrefetch = True
                # This is a late prefetch: there is a demand cache miss for the same address in a different MSHR.
                # This is quite unlikely: needs the core to request the address a cycle before prefetch creation (I think....)
                if isinstance(ll, CRqMissLine) and self.isPrefetch and not ll.cRqIsPrefetch and ll.newLineAddr == self.lineAddr and ll.mshr != self.mshr and not self.isLatePrefetch:
                    self.isLatePrefetch = True
                    self.latePrefetchRelativeCycles = ll.timestamp - self.timestamp

        # If we didn't find a response, discard this line
        if self.discardIf(not self.hit and not self.miss and not self.owned, "no cRq response"):
            return

        # If this is a prefetch that hit or is dependent on another cRq, then it may have been late.       
        if self.isPrefetch and (self.hit or self.owned):
            for ll in reversed(before):
                if isinstance(ll, TimestampedLine):
                    if ll.timestamp + self.MAX_LATE_PREFETCH_ISSUE_CYCLES < self.timestamp:
                        break
                    # There is an earlier miss for the same address.
                    # If it is a demand miss, then this is a late prefetch.
                    # If it is a prefetch miss, then this is probably a duplicate prefetch.
                    if isinstance(ll, CRqMissLine) and ll.newLineAddr == self.lineAddr:
                        if not ll.cRqIsPrefetch:
                            self.isLatePrefetch = True
                            self.latePrefetchRelativeCycles = ll.timestamp - self.timestamp
                        break
        
        # If this is a prefetch that missed, then post-processing for the 
        # CRqMissLine may still deduce that this prefetch was late.  

    def getTotals(self) -> dict[str, int]:
        rt = super().getTotals()
        return rt if self.discard else rt | {
            "Pr"    : int(self.isPrefetch),
            self.op : int(not self.isPrefetch),

            "demand"       : int(not self.isPrefetch),
            "demandHit"    : int(not self.isPrefetch and self.hit),
            "demandMiss"   : int(not self.isPrefetch and self.miss),
            "demandMissLL" : int(not self.isPrefetch and self.miss and not self.cRqResponseLine.hitInLL),
            "demandOwned"  : int(not self.isPrefetch and self.owned),

            "prefetch"       : int(self.isPrefetch),
            "prefetchHit"    : int(self.isPrefetch and self.hit),
            "prefetchMiss"   : int(self.isPrefetch and self.miss),
            "prefetchMissLL" : int(self.isPrefetch and self.miss and not self.cRqResponseLine.hitInLL),
            "prefetchOwned"  : int(self.isPrefetch and self.owned),

            "latePrefetch" : int(self.isLatePrefetch),
            "latePrefetchCreation" : int(self.isLatePrefetch and (self.hit or self.owned)),
            "latePrefetchIssue"    : int(self.isLatePrefetch and self.miss),
            "prefUnderPref"   : int(self.isPrefetchUnderPrefetch),
            "uselessPrefetch" : int(self.isPrefetch and self.isNeverAccessed),
            "uselessPrefetchBecausePerms" : int(self.isPrefetch and self.isNeverAccessed and self.isNeverAccessedBecausePerms),
            "uselessPrefetchDisruption"   : int(self.isPrefetch and self.isNeverAccessed and self.disruptedCache),
            "prefetchDisruption"          : int(self.isPrefetch and self.disruptedCache),
        }
        
    def getDistributions(self) -> dict[str, int]:
        if self.discard:
            return {}
        rt = {
            "mshrRemaining" : self.totalMshr - self.mshrUsed
        }
        if not self.isPrefetch and self.cRqHitLine is not None:
            rt["demandHitCycles"] = self.cRqHitLine.timestamp - self.timestamp
        if self.isLatePrefetch:
            rt["latePrefetchWhenWasDemandMissRelativeToPrefetchCreation"] = self.latePrefetchRelativeCycles
            if self.cRqHitLine is not None: 
                rt["latePrefetchHowMuchEarlierToHit"] = self.cRqHitLine.timestamp - self.timestamp - self.latePrefetchRelativeCycles
        if not self.isPrefetch:
            rt["demandCapSize"] = self.boundsLength
        if not self.isPrefetch and self.cRqHitLine is not None and self.cRqHitLine.nCap > 0:
            rt["demandHasPtrsCapSize"] = self.boundsLength
        return rt



@NonRVFILine.createSubLineType
class CRqHitLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ L1D cRq hit"
    _DATA_REGEX = r"^\d+ L1D cRq hit: mshr: (\d+), addr: (0x[0-9a-f]+), cRq is prefetch: ([01]), wasMiss: ([01]), pipeCs: ([ITSEM]), reqCs: ([ITSEM]), saveCs: ([ITSEM]), op: (Ld|St|Lr|Sc|Amo), nCap: (\d), data: (.*)"

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = CRqHitLine.dataRegex(line)
        self.mshr          = int(reData[0])
        self.addr          = int(reData[1], 0)
        self.lineAddr      = self.addr >> 6
        self.cRqIsPrefetch = bool(reData[2] == "1")
        self.wasMiss       = bool(reData[3] == "1")
        self.pipeCs        = str(reData[4])
        self.reqCs         = str(reData[5])
        self.saveCs        = str(reData[6])
        self.op            = str(reData[7])
        self.nCap          = int(reData[8])
        self.lineData      = str(reData[9])
        # Will be set by a prior CRqCreationLine is this is an immediate hit,
        # or a CRqMissLine/CRqDependencyLine otherwise.
        self.cRqCreationLine = None
        # Will be set by a prior CRqMissLine if this hit was once a miss
        self.cRqMissEvictionLine = None
        # Set in self.postProcess
        self.evictionLine   = None
        self.evictionCycles = None

    def postProcess(self, before, after):
        super().postProcess(before, after)
        if self.discard: return

        # Produce a warning if there was no creation of this cRq
        self.warnIf(self.cRqCreationLine is None, "no creation of cRq")
        if self.cRqCreationLine is not None:
            self.cRqCreationLine.cRqHitLine   = self

        # If this is the refill of a miss, then look for the eventual eviction.
        # Also look for cache disruption during its lifetime.
        if self.wasMiss:
            self.warnIf(self.cRqMissEvictionLine is None, "no cRq eviction for miss")
            for ll in after:
                # There is a cRq miss that is replacing this line
                if isinstance(ll, CRqMissLine) and ll.oldLineAddr == self.lineAddr:
                    # Maybe tell the creation log line that it was never accessed
                    if ll.wasPrefetch and self.cRqCreationLine is not None and not self.cRqCreationLine.isNeverAccessed:
                        self.cRqCreationLine.isNeverAccessed = True
                        self.cRqCreationLine.isNeverAccessedBecausePerms = ll.permsOnly
                    # The miss could actually be a permissions upgrade: don't count this as eviction
                    if ll.permsOnly:
                        continue
                    else:
                        self.evictionLine   = ll
                        break
                # Being evicted by a pRq request
                if isinstance(ll, PRqLine) and ll.lineAddr == self.lineAddr and ll.reqCs == "I":
                    if ll.wasPrefetch and self.cRqCreationLine is not None:
                        self.cRqCreationLine.isNeverAccessed = True
                    self.evictionLine   = ll
                    break       
                # The evicted line is being brought back into the cache.
                # This constitutes cache disruption.
                if isinstance(ll, CRqMissLine) and self.cRqMissEvictionLine is not None and ll.newLineAddr == self.cRqMissEvictionLine.oldLineAddr and self.cRqMissEvictionLine.ramCs != "I":
                    if self.cRqCreationLine is not None:
                        self.cRqCreationLine.disruptedCache   = True
                        self.cRqCreationLine.disruptionCycles = ll.timestamp - self.timestamp

    def getDistributions(self):
        if self.discard:
            return {}
        rt = {}
        if not self.cRqIsPrefetch:
            rt["demandAddr"] = self.addr
        if self.cRqCreationLine is not None and not self.cRqCreationLine.isPrefetch:
            rt["demandNCap"] = self.nCap
        if self.wasMiss:
            rt["missNCap"] = self.nCap
        if self.wasMiss and self.evictionLine is not None:
            rt["evictionCycles"] = self.evictionLine.timestamp - self.timestamp
        return rt
    


@NonRVFILine.createSubLineType
class LLCRqHitLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ LL cRq hit"
    _DATA_REGEX = r"^\d+ LL cRq hit: addr: (0x[0-9a-f]+), cRq is prefetch: ([01]), wasMiss: ([01])"

    def __init__(self, line: str):
        super().__init__(line)
        reData = LLCRqHitLine.dataRegex(line)
        self.addr          = int(reData[0], 0)
        self.lineAddr      = self.addr >> 6
        self.cRqIsPrefetch = bool(reData[1] == "1")
        self.wasMiss       = bool(reData[2] == "1")



@NonRVFILine.createSubLineType
class CRqMissLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ L1D cRq miss"
    _DATA_REGEX = r"^\d+ L1D cRq miss \(([\w ]+)\): mshr: (\d+), addr: (0x[0-9a-f]+), old line addr: (0x[0-9a-f]+), wasPrefetch: ([01]), cRq is prefetch: ([01]), ramCs: ([ITSEM]), reqCs: ([ITSEM]), op: (Ld|St|Lr|Sc|Amo)"

    # How many cycles after a cRq miss to expect the corresponding cache refill
    MAX_PRS_CYCLES = 200

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = CRqMissLine.dataRegex(line)
        self.repType       = str(reData[0])
        self.mshr          = int(reData[1])
        self.addr          = int(reData[2], 0)
        self.newLineAddr   = self.addr >> 6
        self.oldLineAddr   = int(reData[3], 0)
        self.wasPrefetch   = bool(reData[4] == "1")
        self.cRqIsPrefetch = bool(reData[5] == "1")
        self.ramCs         = str(reData[6])
        self.reqCs         = str(reData[7])
        self.op            = str(reData[8])
        self.permsOnly     = self.oldLineAddr == self.newLineAddr and self.ramCs != "I"
        # Set in self.postProcess
        self.hitInLL       = None
        self.cRqHitLine    = None
        # Will be set by a prior CRqCreationLine
        self.cRqCreationLine = None

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        # Produce a warning if there was no creation of this cRq
        self.warnIf(self.cRqCreationLine is None, f"no creation of cRq ({self.repType})")

        # Discard self and creation line
        def discardWithCreationIf(cond: bool, reason: str):
            if self.discardIf(cond, reason):
                if self.cRqCreationLine is not None:
                    self.cRqCreationLine.discardIf(True, reason)
            return cond

        # Look for the cache refill. When a pRs is received for the refill data, the cRqHit rule fires.
        # Also look for the LL hit line (which will tell us if the LL hit or missed).
        # If this cRq is for a prefetch, also see if this prefetch was late, or whether there is a prefetch-under-prefetch.
        foundLLHit  = False
        foundRefill = False
        for ll in after:
            if(isinstance(ll, TimestampedLine)):
                if ll.timestamp > self.timestamp + self.MAX_PRS_CYCLES:
                    break
                # The LL hit line. Assume only one core.
                if isinstance(ll, LLCRqHitLine) and ll.addr == self.addr:
                    if discardWithCreationIf(foundLLHit, "multiple LL hits"): return
                    foundLLHit   = True
                    self.hitInLL = not ll.wasMiss
                # The refill for the miss
                if isinstance(ll, CRqHitLine) and ll.mshr == self.mshr:
                    if discardWithCreationIf(ll.addr != self.addr or ll.cRqIsPrefetch != self.cRqIsPrefetch or not ll.wasMiss, "strange refill"): return
                    foundRefill = True
                    ll.cRqMissEvictionLine = self
                    ll.cRqCreationLine     = self.cRqCreationLine
                    self.cRqHitLine        = ll
                    break
                # This is a prefetch and another prefetch occurred for the same address during refill.
                # Tell that prefetch that it's a duplicate.
                if isinstance(ll, CRqCreationLine) and self.cRqIsPrefetch and ll.isPrefetch and ll.lineAddr == self.newLineAddr:
                    ll.isPrefetchUnderPrefetch = True
                # Look for a demand miss for the address currently being refilled.
                # If the current refill is due to a prefetch, then consider the prefetch as late.
                if isinstance(ll, CRqDependencyLine) and self.cRqIsPrefetch and not ll.cRqIsPrefetch and ll.lineAddr == self.newLineAddr:
                    # It's possible that, if some log was skipped, the cRq is a prefetch but the creation was missed.
                    if self.cRqCreationLine is not None and not self.cRqCreationLine.isLatePrefetch:
                        self.cRqCreationLine.isLatePrefetch = True
                        self.cRqCreationLine.latePrefetchRelativeCycles = ll.timestamp - self.cRqCreationLine.timestamp

        # If we didn't find an L1 or LL hit line, discard this log line and the cRq creator.
        if discardWithCreationIf(not foundRefill, "no pRs"): return
        if discardWithCreationIf(not foundLLHit, "pRs with no LL hit"): return 

    def getTotals(self) -> dict[str, int]:
        rt = super().getTotals()
        if self.discard:
            return rt
        if self.ramCs == "I" or self.oldLineAddr == self.newLineAddr:
            rt[f"{self.ramCs} --({'Pr' if self.cRqIsPrefetch else self.op})--> {self.reqCs}"] = 1
        else:
            rt[f"{self.ramCs} --/{'Pr' if self.cRqIsPrefetch else self.op}/--> {self.reqCs}"] = 1
        return rt
    
    def getDistributions(self) -> dict[str, int]:
        if self.discard:
            return {}
        rt = {"refillCycles" : self.cRqHitLine.timestamp - self.timestamp}
        return rt



@NonRVFILine.createSubLineType
class CRqDependencyLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ L1D cRq dependency"
    _DATA_REGEX = r"^\d+ L1D cRq dependency: mshr: (\d+), depMshr: (\d+), addr: (0x[0-9a-f]+), cRq is prefetch: ([01]), reqCs: ([ITSEM]), op: (Ld|St|Lr|Sc|Amo)"

    # How many cycles after a cRq miss to expect the corresponding cache refill
    MAX_PRS_CYCLES = 200

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = CRqDependencyLine.dataRegex(line)
        self.mshr          = int(reData[0])
        self.depMshr       = int(reData[1])
        self.addr          = int(reData[2], 0)
        self.lineAddr      = self.addr >> 6
        self.cRqIsPrefetch = bool(reData[3] == "1")
        self.reqCs         = str(reData[4])
        self.op            = str(reData[5])
        # Set in self.postProcess
        self.resolveHit     = False
        self.resolveMiss    = False
        self.cRqResolveLine = None
        # Will be set by a prior CRqCreationLine
        self.cRqCreationLine = None

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        # Produce a warning if there was no creation of this cRq
        self.warnIf(self.cRqCreationLine is None, "no creation of cRq")

        # Discard self and creation line
        def discardWithCreationIf(cond: bool, reason: str):
            if self.discardIf(cond, reason):
                if self.cRqCreationLine is not None:
                    self.cRqCreationLine.discardIf(True, reason)
            return cond

        # If this is not a prefetch, look for the dependency to resolve. 
        # Will probably be cache hit, but might be a miss without replacement if c-state is not enough to hit.
        # If this is a prefetch, then it will be dropped here.
        if not self.cRqIsPrefetch:
            for ll in after:
                if(isinstance(ll, TimestampedLine)):
                    if ll.timestamp > self.timestamp + self.MAX_PRS_CYCLES:
                        break
                    # Dependency resolved and hit
                    if isinstance(ll, CRqHitLine) and ll.mshr == self.mshr:
                        if discardWithCreationIf(ll.addr != self.addr or ll.cRqIsPrefetch != self.cRqIsPrefetch, "strange resolution"): return
                        self.resolveHit     = True
                        self.cRqResolveLine = ll
                        ll.cRqCreationLine  = self.cRqCreationLine
                        break
                    # Dependency resolved but permissions missed
                    if isinstance(ll, CRqMissLine) and ll.mshr == self.mshr:
                        if discardWithCreationIf(ll.oldLineAddr != self.lineAddr or ll.addr != self.addr or ll.cRqIsPrefetch != self.cRqIsPrefetch, "strange resolution"): return
                        self.resolveMiss    = True
                        self.cRqResolveLine = ll
                        ll.cRqCreationLine  = self.cRqCreationLine
                        break 
                    # This is a prefetch and another prefetch occurred for the same address during resolution.
                    # Tell that prefetch that it's a duplicate.
                    if isinstance(ll, CRqCreationLine) and self.cRqIsPrefetch and ll.isPrefetch and ll.lineAddr == self.lineAddr:
                        ll.isPrefetchUnderPrefetch = True
            
            # If we didn't find a resolution, discard this log line and the cRq creator.
            if discardWithCreationIf(not self.resolveHit and not self.resolveMiss, "no resolution"): return
        
    def getTotals(self):
        rt = super().getTotals()
        return rt if self.discard else rt | {
            "demand"      : int(not self.cRqIsPrefetch),
            "resolveHit"  : int(self.resolveHit),
            "resolveMiss" : int(self.resolveMiss),
        }
    
    def getDistributions(self):
        if self.discard:
            return {}
        rt = {}
        if not self.cRqIsPrefetch:
            rt["resolveCycles"] = self.cRqResolveLine.timestamp - self.timestamp
        return rt



@NonRVFILine.createSubLineType
class PRqLine(NonRVFILine):

    _TEST_REGEX = r"^\d+ L1D pRq"
    _DATA_REGEX = r"^\d+ L1D pRq: line addr: (0x[0-9a-f]+), wasPrefetch: ([01]), overtakeCRq: ([01]), ramCs: ([ITSEM]), reqCs: ([ITSEM])"

    def __init__(self, line: str) -> None:
        super().__init__(line)
        reData = PRqLine.dataRegex(line)
        self.lineAddr    = int(reData[0], 0)
        self.wasPrefetch = bool(reData[1] == "1")
        self.overtakeCRq = bool(reData[2] == "1")
        self.ramCs       = str(reData[3])
        self.reqCs       = str(reData[4])



@NonRVFILine.createSubLineType
class CapPtrCacheDataArrivalLine(NonRVFILine):
    
    _TEST_REGEX = r"^\d+ Prefetcher reportCacheDataArrival wasMiss"
    _DATA_REGEX = r"^\d+ Prefetcher reportCacheDataArrival wasMiss ([01]) wasPrefetch ([01]) (.*)" 
        
    def __init__(self, line):
        super().__init__(line)
        reData = CapPtrCacheDataArrivalLine.dataRegex(line)
        self.wasMiss     = bool(reData[0] == "1")
        self.wasPrefetch = bool(reData[1] == "1")
        self.lineData    = str(reData[2])



@NonRVFILine.createSubLineType
class CapPtrDataLookupPTableLine(NonRVFILine):
    
    _TEST_REGEX = r"^\d+ Prefetcher reportDataArrival.*caps for prefetch lookups"
    _DATA_REGEX = r"^\d+ Prefetcher reportDataArrival addr ([0-9a-f]+) prefetech ([01]) adding (\d+) caps for prefetch lookups \(clinestartoffset ([0-9a-f]+)\)(.*)" 
        
    def __init__(self, line):
        super().__init__(line)
        reData = CapPtrDataLookupPTableLine.dataRegex(line)
        self.addr             = int(reData[0], 16)
        self.lineAddr         = self.addr >> 6
        self.prefetch         = bool(reData[1] == "1")
        self.nCap             = int(reData[2])
        self.clineStartOffset = int(reData[3], 16)
        self.capData          = str(reData[4])



@NonRVFILine.createSubLineType
class CapPtrDataAddTTableEntryLine(NonRVFILine):
    
    _TEST_REGEX = r"^\d+ Prefetcher reportDataArrival adding training table entry!"
    _DATA_REGEX = r"^\d+ Prefetcher reportDataArrival adding training table entry! access addr ([0-9a-f]+) boundslen \s*(\d+) offset ([0-9a-f]+) prefetch ([01]) pcHash ([0-9a-f]+) ptraddress ([0-9a-f]+) ptrbase ([0-9a-f]+) ptrlength \s*(\d+) tit ([0-9a-f]+) pit ([0-9a-f]+)" 
        
    STACK_SIZE = 0xc0400000
    MAX_TIME_TO_LOOK_FOR_ACCESSES = 2000

    def __init__(self, line):
        super().__init__(line)
        reData = CapPtrDataAddTTableEntryLine.dataRegex(line)
        self.addr         = int(reData[0], 16)
        self.lineAddr     = self.addr >> 6
        self.boundsLength = int(reData[1])
        self.boundsOffset = int(reData[2], 16)
        self.pcHash       = int(reData[3], 16)
        self.prefetch     = bool(reData[4] == "1")
        self.ptrAddress   = int(reData[5], 16)
        self.ptrBase      = int(reData[6], 16)
        self.ptrLength    = int(reData[7])
        self.tit          = int(reData[8], 16)
        self.pit          = int(reData[9], 16)
        # Set by self.postProcess
        self.offsetsAccessed = set()
        self.cacheAlignedOffsetsAccessed = set()
        self.ptrToStackOrAlmighty = self.ptrLength >= self.STACK_SIZE and self.ptrBase == 0

    def postProcess(self, before: Iterable[LogLine], after: Iterable[LogLine]) -> None:
        super().postProcess(before, after)
        if self.discard: return

        for ll in after:
            if isinstance(ll, TimestampedLine):
                if isinstance(ll, CRqCreationLine) and ll.isDemand and ll.boundsBase == self.ptrBase:
                    self.offsetsAccessed.add(ll.boundsOffset)
                    self.cacheAlignedOffsetsAccessed.add(ll.boundsOffset >> 6)
                if ll.timestamp >= self.timestamp + self.MAX_TIME_TO_LOOK_FOR_ACCESSES:
                    break

    def getTotals(self):
        rt = super().getTotals()
        return rt if self.discard else rt | {
            "numNonStack"                : int(not self.ptrToStackOrAlmighty),
        }

    def getDistributions(self):
        rt = super().getDistributions()
        return rt if self.discard else rt | {
            "boundsLength"               : self.boundsLength,
            "ptrLength"                  : self.boundsLength,
            "numOffsetsAccessed"         : len(self.offsetsAccessed),
            "numNonStackOffsetsAccessed" : len(self.offsetsAccessed) if not self.ptrToStackOrAlmighty else 0,
            "numNonStackCacheAlignedOffsetsAccessed" : len(self.cacheAlignedOffsetsAccessed) if not self.ptrToStackOrAlmighty else 0,
        }



@NonRVFILine.createSubLineType
class CapPtrPTUpgradeLine(NonRVFILine):
    
    _TEST_REGEX = r"^\d+ Prefetcher processPtReadUpgrade"
    _DATA_REGEX = r"^\d+ Prefetcher processPtReadUpgrade (hit|miss) pit ([0-9a-f]+) set lastUsedOffset \s*(\d+) to \s*(\d+), changed state to (.*)" 
        
    def __init__(self, line):
        super().__init__(line)
        reData = CapPtrPTUpgradeLine.dataRegex(line)
        self.hit        = reData[0] == "hit"
        self.miss       = reData[0] == "miss"
        self.pit        = int(reData[1], 16)
        self.lastOffset = int(reData[2])
        self.newOffset  = int(reData[3])
        self.state      = reData[4]



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
    def niceReadLines(fp, chunksize=128*1024*1024) -> Iterable[str]:
        leftovers = ""
        while (chunk := fp.read(chunksize).decode("utf-8")):
            lines = (leftovers + chunk).split('\n')
            for line in lines[:-1]:
                yield line
            leftovers = lines[-1]
            del lines
            del chunk

    def __init__(
        self, 
        log: str, 
        skipLines: int | None = None,
        maxLines: int | None = None, 
        lineTypesToPrune: List[type[LogLine]] = [],
        lineTypesToError: List[type[LogLine]] = [],
        RootLogLine: type[LogLine] = LogLine,
        startWhen: Callable[[LogLine], bool] | None = None
    ) -> None:

        self.logLines: deque[LogLine] = deque()
        self.lineTypeCounts: dict[type[LogLine], int] = {}
        started = startWhen == None

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
                # Skip the line if the user specified skipLines and there are lines remaining to skip
                if skipLines is not None and skipLines > 0:
                    skipLines -= 1
                    continue
                # Instantiate the line type
                try:
                    logLine = LineType(line)
                except Exception as e:
                    raise type(e)(f"Failed to load '{log}': {e}")
                # Check if we need to skip this line because we haven't started
                started = started or startWhen(logLine)
                if not started: continue
                # Save the line to logLines
                self.logLines.append(logLine)
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
        for process in ["Pre", "Post"]:
            before = deque()
            after  = self.logLines
            while len(after) != 0:
                current = after.popleft()
                if process == "Pre":
                    current.preProcess(before, after)
                else:
                    current.postProcess(before, after)
                before.append(current)
                if(len(before) % 10000 == 0):
                    print(f"\r{process}-processed {len(before)} log lines", end="")
            del after
            self.logLines = before
            print(f"\r{process}-processed {len(self.logLines)} log lines")

        # Calculate totals and dists
        self.recalculateTotalsAndDists()



    def recalculateTotalsAndDists(self):
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



    def plotDist(
        self,
        LineType: type[LogLine],
        distName: str,
        figax = None,
        xsAreAddresses = None
    ):
        fig, ax = figax if figax is not None else plt.subplots(figsize=(8, 6))
        data = self.dists[LineType][distName]
        if xsAreAddresses is None:
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
        xtickIntervalOpts  = [10**i for i in range(14, 4, -1)] + [50000,20000,10000,5000,2000,1000,100,50,25,20,10,5,2,1] if not xsAreAddresses else [2**x for x in range(32,-1,-1)]
        xtickIdealCount    = 6 if not xsAreAddresses else 12
        xtickExactInterval = max(1, (maxxs-minxs)//xtickIdealCount)
        xtickNiceInterval  = next(i for i in xtickIntervalOpts if i <= xtickExactInterval)
        xtickMin = minxs-(minxs%xtickNiceInterval)
        xtickMax = maxxs+xtickNiceInterval
        ax.bar(xs, ys, color='skyblue', width=1)
        ax.set_title(f"{LineType.__name__}: {distName}")
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
        return fig,ax

        

    def plotDists(self) -> List:
        figaxs = []
        for LineType, dists in self.dists.items():
            for dataName in dists:
                figaxs.append(self.plotDist(LineType, dataName))
        return figaxs