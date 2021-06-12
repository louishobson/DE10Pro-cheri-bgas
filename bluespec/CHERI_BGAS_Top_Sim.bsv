package CHERI_BGAS_Top_Sim;

import FIFOF :: *;
import AXI4 :: *;
import AXI4Lite :: *;
import AXI4_AXI4Lite_Bridges :: *;
import BlueUtils :: *;
import SourceSink :: *;
import Connectable :: *;
import CHERI_BGAS_Top :: *;
import DE10Pro_bsv_shell :: *;
import Recipe :: *;

// Concrete parameters definitions
// -------------------------------

`define H2F_LW_ADDR   21 // from 20 (1MB) to 21 (2MB)
`define H2F_LW_DATA   32
`define H2F_LW_AWUSER  0
`define H2F_LW_WUSER   0
`define H2F_LW_BUSER   0
`define H2F_LW_ARUSER  0
`define H2F_LW_RUSER   0

`define H2F_ID       4
`define H2F_ADDR    32 // from 20 (1MB) to 32 (4GB)
`define H2F_DATA   128 // 32, 64 or 128
`define H2F_AWUSER   0
`define H2F_WUSER    0
`define H2F_BUSER    0
`define H2F_ARUSER   0
`define H2F_RUSER    0

`define F2H_ID       4
`define F2H_ADDR    32 // from 20 (1MB) to 40 (1TB)
`define F2H_DATA   128
`define F2H_AWUSER   0
`define F2H_WUSER    0
`define F2H_BUSER    0
`define F2H_ARUSER   0
`define F2H_RUSER    0

// DDR AXI ports parameters

`define DRAM_ID       7
`define DRAM_ADDR    32
`define DRAM_DATA   128
`define DRAM_AWUSER   0
`define DRAM_WUSER    0
`define DRAM_BUSER    0
`define DRAM_ARUSER   0
`define DRAM_RUSER    0

module mkDriveAXILite (AXI4Lite_Master #( `H2F_LW_ADDR
                                        , `H2F_LW_DATA
                                        , `H2F_LW_AWUSER
                                        , `H2F_LW_WUSER
                                        , `H2F_LW_BUSER
                                        , `H2F_LW_ARUSER
                                        , `H2F_LW_RUSER ));

  let delayReg <- mkRegU;
  function recipeDelay (delay) = rSeq ( rBlock (
    action delayReg <= delay; endaction
    , rWhile ( delayReg > 0, rAct ( action delayReg <= delayReg - 1; endaction))
  ));

  let shim <- mkAXI4LiteShim;
  let debugBaseAddr = 32'hf9000000;
  let resReg <- mkRegU; // reg used by debug module reads and writes for return
                        // values. Must be explicitly handled by the caller

  function readDebugReg (verbosity, idx) = rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting readDebugReg, idx = "
                             , $time
                             , fshow (idx))))
    , shim.slave.ar.put (AXI4Lite_ARFlit {
        araddr: debugBaseAddr + zeroExtend ({idx, 2'b00})
      , arprot: 0
      , aruser: 0
      })
    , action
        let val <- get (shim.slave.r);
        //resFF.enq (val.rdata);
        resReg <= val.rdata;
      endaction
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended readDebugReg, value returned = "
                             , $time
                             , fshow (resReg))))
    ) );
  function writeDebugReg (verbosity, idx, data) = rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting writeDebugReg, idx = "
                             , $time
                             , fshow (idx)
                             , ", data = "
                             , fshow (data))))
    , action
        shim.slave.aw.put (AXI4Lite_AWFlit {
          awaddr: debugBaseAddr + zeroExtend ({idx, 2'b00})
        , awprot: 0
        , awuser: 0
        });
        shim.slave.w.put (AXI4Lite_WFlit {
          wdata: data
        , wstrb: ~0
        , wuser: 0
        });
      endaction
    , shim.slave.b.drop
    , rWhen ( verbosity > 0
            , rAct ($display ("%0t - Ended writeDebugReg", $time)))
    ) );

  function Recipe debugTooobaReset (Integer verbosity, Bool running) =
    rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting debugTooobaReset, running = "
                             , $time
                             , fshow (running))))
    , writeDebugReg (verbosity - 1, 7'h10, running ? 'h00000003 : 'h80000003)
    , recipeDelay (5)
    , writeDebugReg (verbosity - 1, 7'h10, running ? 'h00000001 : 'h80000001)
    , recipeDelay (500)
    , readDebugReg (verbosity - 1, 7'h11)
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended debugTooobaReset, value returned = "
                             , $time
                             , fshow (resReg))))
    ) );

  function Recipe debugTooobaMemRead (Integer verbosity, Bit #(64) addr) =
    rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting debugTooobaMemRead, addr = "
                             , $time
                             , fshow (addr))))
    , writeDebugReg (verbosity - 1, 7'h17, 'h003207b0)
    , writeDebugReg (verbosity - 1, 7'h38, 'h00150000)
    , writeDebugReg (verbosity - 1, 7'h3a, truncateLSB (addr))
    , writeDebugReg (verbosity - 1, 7'h39, truncate (addr))
    , readDebugReg (verbosity - 1, 7'h3c)
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended debugTooobaMemRead, value returned = "
                             , $time
                             , fshow (resReg))))
    ) );

  function Recipe debugTooobaMemWrite ( Integer verbosity
                                      , Bit #(64) addr
                                      , Bit #(32) data) =
    rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting debugTooobaMemWrite, addr = "
                             , $time
                             , fshow (addr)
                             , ", data = "
                             , fshow (data))))
    , writeDebugReg (verbosity - 1, 7'h17, 'h003207b0)
    , writeDebugReg (verbosity - 1, 7'h38, 'h00050000)
    , writeDebugReg (verbosity - 1, 7'h3a, truncateLSB (addr))
    , writeDebugReg (verbosity - 1, 7'h39, truncate (addr))
    , writeDebugReg (verbosity - 1, 7'h3c, data)
    , readDebugReg (verbosity - 1, 7'h38)
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended debugTooobaMemWrite, value returned = "
                             , $time
                             , fshow (resReg))))
    ) );

  Integer verbosity = 2;
  PulseWire done <- mkPulseWire;
  Recipe r = rSeq ( rBlock (
      debugTooobaReset (verbosity, False)
    //, debugTooobaMemWrite (verbosity, 'h80008000, 'hdeadbeef)
    //, debugTooobaMemRead (verbosity, 'h80008000)
    , done.send
    ));
  RecipeFSM m <- mkRecipeFSM (r);
  // Start runing the recipe
  rule run;
    $display("starting at time %0t", $time);
    $display("------------------------------------------");
    m.trigger;
  endrule

  // On the recipe's last cyle, terminate simulation
  rule endSim (done);
    $display("------------------------------------------");
    $display("finishing at time %0t", $time);
    $finish(0);
  endrule
  return debugAXI4Lite_Master ( truncateAddrFieldsMasterLite (shim.master)
                              , $format ("axilite driver"));
endmodule

module mkFakeDDR (AXI4_Slave #( `DRAM_ID
                              , `DRAM_ADDR
                              , `DRAM_DATA
                              , `DRAM_AWUSER
                              , `DRAM_WUSER
                              , `DRAM_BUSER
                              , `DRAM_ARUSER
                              , `DRAM_RUSER ));

  Integer verbosity = 2;
  //let memLite <- mkAXI4LiteMem ('hffffffff, Invalid);
  //return debugAXI4_Slave (fromAXI4LiteToAXI4_Slave (memLite), $format ("fake ddr"));
  let rFF <- mkUGFIFOF;
  let rFlits <- mkReg (0);
  let awFF <- mkUGFIFOF;
  let wFF <- mkUGFIFOF;
  let wFlits <- mkReg (0);
  let allTheMemoryInTheWorld <- mkRegU;
  interface Sink ar;
    method canPut = rFF.notFull;
    method put = rFF.enq;
  endinterface
  interface Source r;
    method canPeek = rFF.notEmpty;
    method peek = AXI4_RFlit { rid: rFF.first.arid
                             , rresp: OKAY
                             , rdata: allTheMemoryInTheWorld
                             , rlast: (rFF.first.arlen == rFlits)
                             , ruser: 0 };
    method drop = action
      $display ("%0t - mkFakeDDR answers ", $time, fshow (rFF.first));
      if (rFF.first.arlen == rFlits) begin
        $display ("%0t - mkFakeDDR consumes ", $time, fshow (rFF.first));
        rFF.deq;
        rFlits <= 0;
      end else rFlits <= rFlits + 1;
    endaction;
  endinterface
  interface Sink aw;
    method canPut = awFF.notFull;
    method put = awFF.enq;
  endinterface
  interface Sink w;
    method canPut = wFF.notFull;
    method put (x) = action
      wFF.enq (x);
      allTheMemoryInTheWorld <= x.wdata;
    endaction;
  endinterface
  interface Source b;
    method canPeek = awFF.notEmpty && wFF.notEmpty;
    method peek = AXI4_BFlit { bid: awFF.first.awid
                             , bresp: OKAY
                             , buser: 0 };
    method drop = action
      if (awFF.first.awlen == wFlits) begin
        $display ("%0t - mkFakeDDR consumes ", $time, fshow (awFF.first));
        awFF.deq;
        wFlits <= 0;
      end else wFlits <= wFlits + 1;
      wFF.deq;
      $display ("%0t - mkFakeDDR consumes ", $time, fshow (wFF.first));
    endaction;
  endinterface

endmodule

module mkCHERI_BGAS_Top_Sim (Empty);
  DE10ProIfc cheri_bgas_top <- mkCHERI_BGAS_Top;
  AXI4Lite_Master #( `H2F_LW_ADDR
                   , `H2F_LW_DATA
                   , `H2F_LW_AWUSER
                   , `H2F_LW_WUSER
                   , `H2F_LW_BUSER
                   , `H2F_LW_ARUSER
                   , `H2F_LW_RUSER ) axiLiteDriver <- mkDriveAXILite;
  AXI4_Slave #( `DRAM_ID
              , `DRAM_ADDR
              , `DRAM_DATA
              , `DRAM_AWUSER
              , `DRAM_WUSER
              , `DRAM_BUSER
              , `DRAM_ARUSER
              , `DRAM_RUSER ) fakeDDRB <- mkFakeDDR;

  mkConnection (cheri_bgas_top.axls_h2f_lw, axiLiteDriver);
  //mkConnection (cheri_bgas_top.axs_h2f, culDeSac);
  //mkConnection (cheri_bgas_top.axm_f2h, culDeSac);
  mkConnection (cheri_bgas_top.axm_ddrb, fakeDDRB);
  //mkConnection (cheri_bgas_top.axm_ddrc, culDeSac);
  //mkConnection (cheri_bgas_top.axm_ddrd, culDeSac);
endmodule

endpackage
