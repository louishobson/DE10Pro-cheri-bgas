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
import AXI4_AXI4Lite_Bridges :: *;
import Vector :: *;
import Clocks :: *;
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

`define DRAM_ID       8
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

typedef DE10Pro_bsv_shell_Sig #( `H2F_LW_ADDR
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
                               , `DRAM_RUSER ) DE10ProIfcSig;

module mkCHERI_BGAS_Top (DE10ProIfc)
  provisos ( NumAlias #(bus_mid, TAdd #(Wd_MId, 1)) // id width out of the core
           , NumAlias #(bus_sid, TAdd #(Wd_MId, 2)) // cope with 2 masters only
           );

  // declare cpu core with WindCoreMid interface
  //////////////////////////////////////////////////////////////////////////////

  Clock clk <- exposeCurrentClock;
  Reset rst <- exposeCurrentReset;
  let newRst <- mkReset (0, True, clk, reset_by rst);
  Tuple2 #( PulseWire
          , CoreW_IFC#(N_External_Interrupt_Sources)) both
    <- mkCoreW_reset (rst, reset_by newRst.new_rst);
  match {.otherRst, .midCore} = both;
  rule rl_forward_debug_reset (otherRst);
    newRst.assertReset;
  endrule

  // declare extra AXI4 lite ctrl subordinates
  //////////////////////////////////////////////////////////////////////////////

  // fake 16550
  Tuple2 #(
    Tuple2 #( AXI4Lite_Slave #( `H2F_LW_ADDR, `H2F_LW_DATA
                              , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                              , `H2F_LW_ARUSER, `H2F_LW_RUSER)
            , ReadOnly #(Bool) )
  , Tuple2 #( AXI4Lite_Slave #( `H2F_LW_ADDR, `H2F_LW_DATA
                              , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                              , `H2F_LW_ARUSER, `H2F_LW_RUSER)
            , ReadOnly #(Bool) )) ifcs
    <- mkAXI4_Fake_16550_Pair (reset_by newRst.new_rst);
  match {{.s0, .irq0}, {.s1, .irq1}} = ifcs;

  // prepare AXI4 managers
  //////////////////////////////////////////////////////////////////////////////

  // convert to WindCoreHi interface and map additional AXI4 Lite subordinates
  let core <- windCoreMid2Hi_Core (
                midCore
              , cons ( tuple2 ( s0
                              , Range { base: 'h0003_0000, size: 'h0000_1000 } )
                     , nil )
              , cons (irq0, nil)
              , reset_by newRst.new_rst);

  // gather all managers
  Vector #(2, AXI4_Master #(bus_mid, Wd_Addr, Wd_Data, 0, 0, 0, 0, 0))
    ms;
  ms[0] = core.manager_0;
  ms[1] = core.manager_1;

  // prepare AXI4 subordinates
  //////////////////////////////////////////////////////////////////////////////

  // prepare fake 16550
  AXI4_Shim #(bus_sid, `H2F_LW_ADDR, `H2F_LW_DATA
                     , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                     , `H2F_LW_ARUSER, `H2F_LW_RUSER)
    fake16550DeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  mkConnection (fake16550DeBurst.master, s1, reset_by newRst.new_rst);
  AXI4_Slave #(bus_sid, Wd_Addr, Wd_Data, 0, 0, 0, 0, 0)
    fake16550_s <- fmap ( truncate_AXI4_Slave_addr
                        , toWider_AXI4_Slave ( fake16550DeBurst.slave
                                             , reset_by newRst.new_rst));

  // prepare bootrom
  AXI4_Shim #(bus_sid, Wd_Addr, Wd_Data, 0, 0, 0, 0, 0)
    fakeBootRomDeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  AXI4_Slave #(bus_sid, Wd_Addr, Wd_Data, 0, 0, 0, 0, 0)
    fakeBootRom <- mkPerpetualZeroAXI4Slave (reset_by newRst.new_rst);
  mkConnection ( fakeBootRomDeBurst.master, fakeBootRom
               , reset_by newRst.new_rst);

  // prepare ddrb channel
  AXI4_Shim #(bus_sid, Wd_Addr, Wd_Data, 0, 0, 0, 0, 0)
    ddrbDeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  let ddrb_mngr <- fmap ( truncate_AXI4_Master_addr
                        , toWider_AXI4_Master ( ddrbDeBurst.master
                                              , reset_by newRst.new_rst));

  // gather all subordinates
  Vector #(3, AXI4_Slave #(bus_sid, Wd_Addr, Wd_Data, 0, 0, 0, 0, 0))
    ss;
  ss[0] = ddrbDeBurst.slave;
  ss[1] = fake16550_s;
  ss[2] = debugAXI4_Slave (fakeBootRomDeBurst.slave, $format ("fake bootRom"));

  // build route
  SoC_Map_IFC soc_map <- mkSoC_Map (reset_by newRst.new_rst);
  function Vector #(3, Bool) route (Bit #(Wd_Addr) addr);
    Vector #(3, Bool) x = unpack (3'b000);
    if (inRange (soc_map.m_boot_rom_addr_range, addr))
      x[2] = True;
    else if (inRange (soc_map.m_uart16550_0_addr_range, addr))
      x[1] = True;
    else if (   inRange (soc_map.m_ddr4_0_uncached_addr_range, addr)
             || inRange (soc_map.m_ddr4_0_cached_addr_range, addr) )
      x[0] = True;
    return x;
  endfunction

  // wire it all up
  mkAXI4Bus (route, ms, ss, reset_by newRst.new_rst);

  // prepare irq vector
  //////////////////////////////////////////////////////////////////////////////

  Vector #(32, ReadOnly #(Bool)) irqs = replicate (interface ReadOnly;
                                                     method _read = False;
                                                   endinterface);
  // the fake 16550 irq
  irqs[0] = irq1;

  // interface
  interface axls_h2f_lw = core.control_subordinate;
  interface axs_h2f = core.subordinate_0;
  interface axm_f2h = culDeSac;
  interface axm_ddrb = ddrb_mngr;
  interface axm_ddrc = culDeSac;
  interface axm_ddrd = culDeSac;
  interface ins_irq0 = irqs;
endmodule

(* synthesize *)
module mkCHERI_BGAS_Top_Sig (DE10ProIfcSig);
  let noSigIfc <- mkCHERI_BGAS_Top;
  let sigIfc <- toDE10Pro_bsv_shell_Sig (noSigIfc);
  return sigIfc;
endmodule

endpackage
