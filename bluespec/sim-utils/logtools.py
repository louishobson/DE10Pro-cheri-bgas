import sys
from parselog import *
import os



def getMaxRVFI(run, prefetcherLocation, benchmark):
    prefetcher, location = prefetcherLocation
    logPath  = f"results/{run}-{prefetcher}-{location}/{benchmark}/sim_stdout.gz"
    lastRVFI = 0
    lastTime = 0
    sys.stdout.write(f"Scanning {run}-{prefetcher}-{location}/{benchmark}...\n")
    with LogParser.openMaybeGZip(logPath) as fp:
        for chunk in LogParser.niceReadChunk(fp, chunksize=10*1024*1024):
            for line in reversed(chunk):
                line = line.strip()
                if RVFILine.deduceLineType(line) == RVFILine:
                    ll = RVFILine(line)
                    rvfi = ll.rvfi
                    while rvfi < lastRVFI:
                        rvfi += 2**23
                    lastRVFI = rvfi
                    lastTime = ll.timestamp
                    break
    sys.stdout.write(f"... scanned {run}-{prefetcher}-{location}/{benchmark}: {lastRVFI} @ {lastTime}\n")
    sys.stdout.flush()
    return run, prefetcherLocation, benchmark, lastRVFI



def loadPrefetcherData(run, prefetcherLocation, benchmark, minRvfi, maxRvfi):
    prefetcher, location = prefetcherLocation
    logPath = f"results/{run}-{prefetcher}-{location}/{benchmark}/sim_stdout.gz"
    lp = None
    sys.stdout.write(f"Loading {run}-{prefetcher}-{location}/{benchmark}...\n")
    sys.stdout.flush()
    if os.path.isfile(logPath):
        lp = LogParser(
            log=logPath, 
            lineTypesToPrune=[None, NonRVFILine],
            lineTypesToError=[TimestampedLine],
            RootLogLine=TimestampedLine,
            startWhen=(lambda ll: isinstance(ll, RVFILine) and ll.rvfi >= minRvfi),
            stopWhen=(lambda ll: isinstance(ll, RVFILine) and ll.rvfi >= maxRvfi),
            silent=True
        );
    else:
        raise ValueError(f"No such path {logPath}")
    sys.stdout.write(f"... loaded {run}-{prefetcher}-{location}/{benchmark}\n")
    return run, prefetcherLocation, benchmark, lp



def getDuration(run, prefetcherLocation, benchmark, minRvfi, maxRvfi):
    prefetcher, location = prefetcherLocation
    logPath  = f"results/{run}-{prefetcher}-{location}/{benchmark}/sim_stdout.gz"
    lastRvfi = 0
    startTime = None
    endTime = None
    sys.stdout.write(f"Loading {run}-{prefetcher}-{location}/{benchmark}...\n")
    with LogParser.openMaybeGZip(logPath) as fp:
        for chunk in LogParser.niceReadChunk(fp, chunksize=10*1024*1024):
            chunkDone = False
            for line in reversed(chunk):
                line = line.strip()
                if RVFILine.deduceLineType(line) == RVFILine:
                    ll = RVFILine(line)
                    rvfi = ll.rvfi
                    while rvfi < lastRvfi:
                        rvfi += 2**23
                    if (startTime is not None or rvfi < minRvfi) and rvfi < maxRvfi:
                        chunkDone = True
                        lastRvfi = rvfi
                    break
            if chunkDone:
                continue
            for line in chunk:
                line = line.strip()
                if RVFILine.deduceLineType(line) == RVFILine:
                    ll = RVFILine(line)
                    rvfi = ll.rvfi
                    while rvfi < lastRvfi:
                        rvfi += 2**23
                    lastRvfi = rvfi
                    if startTime is None and rvfi == minRvfi:
                        startTime = ll.timestamp
                    if rvfi == maxRvfi:
                        endTime = ll.timestamp
                        break
            if endTime is not None:
                break
    sys.stdout.write(f"... loaded {run}-{prefetcher}-{location}/{benchmark}: {startTime} -- {endTime} \n")
    sys.stdout.flush()
    return run, prefetcherLocation, benchmark, (endTime - startTime)







def nDeepDict(n, Leaf=dict):
        if n == 0:
            return Leaf()
        return defaultdict(lambda: nDeepDict(n-1, Leaf))

def toDict(defdict):
    if isinstance(defdict, defaultdict):
        return {k: toDict(v) for (k, v) in defdict.items()}
    return defdict