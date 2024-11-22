import Cap2024 :: *;
import Cap2024_11 :: *;
import Cap2024_11_Decode_FastFSM :: *;
import GetPut :: *;
import SourceSink :: *;
import FIFOF :: *;
import Tests :: *;

(* synthesize *)
module mkCap2024_11_Decode_FastFSM_Tb(CapDecodeTb#(Cap2024_11));
    FIFOF#(Cap2024_11) in <- mkFIFOF;
    FIFOF#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) out <- mkFIFOF;
    mkFastFSMCapDecode(toGet(in), toPut(out));

    interface stimulusIn = toSink(in);
    interface stimulusOut = toSource(out);
endmodule
