import Cap2024 :: *;
import Cap2024_11 :: *;
import Cap2024_11_Decode_Comb :: *;
import GetPut :: *;
import SourceSink :: *;
import FIFOF :: *;
import Tests :: *;

(* synthesize *)
module mkCap2024_11_Decode_Comb_Tb(CapDecodeTb#(Cap2024_11));
    FIFOF#(Cap2024_11) in <- mkFIFOF;
    FIFOF#(CapCheckResult#(Tuple2#(CapPerms, CapRange))) out <- mkFIFOF;
    mkCombCapDecode(toGet(in), toPut(out));

    interface stimulusIn = toSink(in);
    interface stimulusOut = toSource(out);
endmodule
