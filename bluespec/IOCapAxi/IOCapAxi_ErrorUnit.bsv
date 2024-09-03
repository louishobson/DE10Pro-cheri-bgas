import Vector :: *;

interface ErrorUnit#(type t_err, numeric type n_errs);
    method Action assertError(t_err e);
    interface ReadOnly#(Bit#(n_errs)) errs;
endinterface

module mkErrorUnit(ErrorUnit#(t_err, n_errs)) provisos (Bits#(t_err, n_err_bits), NumAlias#(n_errs, TExp#(n_err_bits)), FShow#(t_err));
    Reg#(Bit#(n_errs)) encounteredErr <- mkReg(0);
    Vector#(n_errs, PulseWire) wantNotifyErr <- replicateM(mkPulseWireOR);

    rule checkErrs;
        Bit#(n_errs) newErrs = encounteredErr;
        for (Integer i = 0; i < valueOf(n_errs); i = i + 1) begin
            if (wantNotifyErr[i]) begin
                newErrs[i] = 1;
            end
        end
        encounteredErr <= newErrs;
    endrule

    method Action assertError(t_err e);
        Bit#(n_err_bits) e_bits = pack(e);
        wantNotifyErr[e_bits].send();
        $error("ERROR - Encountered error ", fshow(e));
    endmethod

    interface errs = regToReadOnly(asIfc(encounteredErr));
endmodule

module mkSingleErrorUnit(ErrorUnit#(Bit#(0), 1));
    Reg#(Bit#(1)) encounteredErr <- mkReg(0);
    PulseWire wantNotifyErr <- mkPulseWireOR;

    rule checkErrs;
        if (wantNotifyErr) begin
            encounteredErr <= 1;
        end
    endrule

    method Action assertError(t_err e);
        wantNotifyErr.send();
        $error("Encountered error");
    endmethod

    interface errs = regToReadOnly(asIfc(encounteredErr));
endmodule