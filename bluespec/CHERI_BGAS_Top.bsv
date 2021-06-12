package CHERI_BGAS_Top;

import DE10Pro_bsv_shell :: *;
import WindCoreInterface :: *;
import AXI4_Fake_16550 :: *;
import Routable :: *;
import SoC_Map :: *;
import Fabric_Defs :: *;
import CoreW :: *;
import AXI4 :: *;
import AXI4Lite :: *;
import Vector :: *;
import Connectable :: *;

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

typedef DE10Pro_bsv_shell #( `H2F_LW_ADDR
                           , `H2F_LW_DATA
                           , `H2F_LW_AWUSER
                           , `H2F_LW_WUSER
                           , `H2F_LW_BUSER
                           , `H2F_LW_ARUSER
                           , `H2F_LW_RUSER
                           , `H2F_ID
                           , `H2F_ADDR
                           , `H2F_DATA
                           , `H2F_AWUSER
                           , `H2F_WUSER
                           , `H2F_BUSER
                           , `H2F_ARUSER
                           , `H2F_RUSER
                           , `F2H_ID
                           , `F2H_ADDR
                           , `F2H_DATA
                           , `F2H_AWUSER
                           , `F2H_WUSER
                           , `F2H_BUSER
                           , `F2H_ARUSER
                           , `F2H_RUSER
                           , `DRAM_ID
                           , `DRAM_ADDR
                           , `DRAM_DATA
                           , `DRAM_AWUSER
                           , `DRAM_WUSER
                           , `DRAM_BUSER
                           , `DRAM_ARUSER
                           , `DRAM_RUSER
                           , `DRAM_ID
                           , `DRAM_ADDR
                           , `DRAM_DATA
                           , `DRAM_AWUSER
                           , `DRAM_WUSER
                           , `DRAM_BUSER
                           , `DRAM_ARUSER
                           , `DRAM_RUSER
                           , `DRAM_ID
                           , `DRAM_ADDR
                           , `DRAM_DATA
                           , `DRAM_AWUSER
                           , `DRAM_WUSER
                           , `DRAM_BUSER
                           , `DRAM_ARUSER
                           , `DRAM_RUSER ) DE10ProIfc;

typedef DE10Pro_bsv_shell_Synth #( `H2F_LW_ADDR
                                 , `H2F_LW_DATA
                                 , `H2F_LW_AWUSER
                                 , `H2F_LW_WUSER
                                 , `H2F_LW_BUSER
                                 , `H2F_LW_ARUSER
                                 , `H2F_LW_RUSER
                                 , `H2F_ID
                                 , `H2F_ADDR
                                 , `H2F_DATA
                                 , `H2F_AWUSER
                                 , `H2F_WUSER
                                 , `H2F_BUSER
                                 , `H2F_ARUSER
                                 , `H2F_RUSER
                                 , `F2H_ID
                                 , `F2H_ADDR
                                 , `F2H_DATA
                                 , `F2H_AWUSER
                                 , `F2H_WUSER
                                 , `F2H_BUSER
                                 , `F2H_ARUSER
                                 , `F2H_RUSER
                                 , `DRAM_ID
                                 , `DRAM_ADDR
                                 , `DRAM_DATA
                                 , `DRAM_AWUSER
                                 , `DRAM_WUSER
                                 , `DRAM_BUSER
                                 , `DRAM_ARUSER
                                 , `DRAM_RUSER
                                 , `DRAM_ID
                                 , `DRAM_ADDR
                                 , `DRAM_DATA
                                 , `DRAM_AWUSER
                                 , `DRAM_WUSER
                                 , `DRAM_BUSER
                                 , `DRAM_ARUSER
                                 , `DRAM_RUSER
                                 , `DRAM_ID
                                 , `DRAM_ADDR
                                 , `DRAM_DATA
                                 , `DRAM_AWUSER
                                 , `DRAM_WUSER
                                 , `DRAM_BUSER
                                 , `DRAM_ARUSER
                                 , `DRAM_RUSER ) DE10ProIfcSynth;

module mkCHERI_BGAS_Top (DE10ProIfc);
  // declare extra AXI4 lite subordinates
  // fake 16550
  Tuple2 #(
    Tuple2 #( AXI4Lite_Slave #( `H2F_LW_ADDR, `H2F_LW_DATA
                              , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                              , `H2F_LW_ARUSER, `H2F_LW_RUSER)
            , ReadOnly #(Bool) )
  , Tuple2 #( AXI4Lite_Slave #( `H2F_LW_ADDR, `H2F_LW_DATA
                              , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                              , `H2F_LW_ARUSER, `H2F_LW_RUSER)
            , ReadOnly #(Bool) )) ifcs <- mkAXI4_Fake_16550_Pair;
  match {{.s0, .irq0}, {.s1, .irq1}} = ifcs;
  // declare cpu core with WindCoreMid interface
  CoreW_IFC#(N_External_Interrupt_Sources) midCore <- mkCoreW;
  // convert to WindCoreHi interface and mapp additional AXI4 Lite subordinates
  let core <- windCoreMid2Hi_WithSubordinates (
                midCore
              , tuple2 ( cons (s0, nil)
                       , cons ( Range { base: 'h0003_0000, size: 'h0000_1000 }
                              , nil )));
  // apply transformations to ddrb channel
  AXI4_Shim #(TAdd#(Wd_MId, 2), Wd_Addr, Wd_Data, 0, 0, 0, 0, 0) deBurst <-
    mkBurstToNoBurst;
  let ddrb_mngr <-
    fmap (truncateAddrFieldsMaster, toWider_AXI4_Master (deBurst.master));
  // merge cached / uncached ddrb traffic
  Vector #(2, AXI4_Master #(TAdd#(Wd_MId, 1), Wd_Addr, Wd_Data, 0, 0, 0, 0, 0))
    ddrb_ms;
  ddrb_ms[0] = core.manager_0;
  ddrb_ms[1] = core.manager_1;
  Vector #(1, AXI4_Slave #(TAdd#(Wd_MId, 2), Wd_Addr, Wd_Data, 0, 0, 0, 0, 0))
    ddrb_ss;
  ddrb_ss[0] = deBurst.slave;
  Vector #(1, Bool) dfltRoute;
  dfltRoute[0] = True;
  mkAXI4Bus (constFn (dfltRoute), ddrb_ms, ddrb_ss);
  // interface
  interface axls_h2f_lw = core.control_subordinate;
  interface axs_h2f = culDeSac; //core.subordinate_0;
  interface axm_f2h = culDeSac;
  interface axm_ddrb = ddrb_mngr;
  interface axm_ddrc = culDeSac;
  interface axm_ddrd = culDeSac;
endmodule

(* synthesize *)
module mkCHERI_BGAS_Top_Synth (DE10ProIfcSynth);
  let noSynthIfc <- mkCHERI_BGAS_Top;
  let synthIfc <- toDE10Pro_bsv_shell_Synth (noSynthIfc);
  return synthIfc;
endmodule

endpackage
