import BlueAXI4 :: *;

/*

// TODO refactor the CreditValve into a more generic hardware blob which takes a predicate and uses that to tell if it should take/drop an input.

typedef enum {
  Take,
  Drop
} FlitAction deriving (Bits, Eq, FShow);

typeclass FalliblePredRoute #(type req_t, type rsp_t)
  module mkFallibleRouteSlave #(function ActionValue#(FlitAction) (req_t))(Slave #(req_t, rsp_t));
endtypeclass

instance FallibleRoute#( AXI4_WriteFlit#(sid_, addr_, data_, awuser_, wuser_)
                       , AXI4_BFlit#(sid_, buser_));
  module mkNoRouteSlave (Slave #( AXI4_WriteFlit#(sid_, addr_, data_, awuser_, wuser_)
                                , AXI4_BFlit#(sid_, buser_)));
    let awidReg <- mkRegU;
    let rspFF <- mkFIFOF;
    interface req = interface Sink;
      method canPut = rspFF.notFull;
      method put (x) if (rspFF.notFull) = action
        let currentAwid = awidReg;
        case (x) matches
          tagged FirstFlit {.aw, ._}: currentAwid = aw.awid;
        endcase
        if (isLast (x)) rspFF.enq (AXI4_BFlit{ bid: currentAwid
                                               , bresp: DECERR
                                               , buser: ? });
        awidReg <= currentAwid;
      endaction;
    endinterface;
    interface rsp = toSource (rspFF);
  endmodule
endinstance

instance FallibleRoute#( AXI4_ARFlit#(sid_, addr_, aruser_)
                       , AXI4_RFlit#(sid_, data_, ruser_));
  module mkNoRouteSlave (Slave #( AXI4_ARFlit#(sid_, addr_, aruser_)
                                , AXI4_RFlit#(sid_, data_, ruser_)));
    Reg#(AXI4_ARFlit#(sid_, addr_, aruser_)) currentReq <- mkRegU;
    Reg#(Bit#(TAdd#(SizeOf#(AXI4_Len), 1)))   flitCount <- mkReg(0);
    interface req = interface Sink;
      method canPut = flitCount == 0;
      method put (x) if (flitCount == 0) = action
        currentReq <= x;
        flitCount  <= zeroExtend(x.arlen) + 1;
      endaction;
    endinterface;
    interface rsp = interface Source;
      method canPeek = flitCount != 0;
      method peek if (flitCount != 0) = AXI4_RFlit{ rid: currentReq.arid
                                                  , rdata: ?
                                                  , rresp: DECERR
                                                  , rlast: flitCount == 1
                                                  , ruser: ? };
      method drop if (flitCount != 0) = action
        flitCount <= flitCount - 1;
      endaction;
    endinterface;
  endmodule
endinstance

*/