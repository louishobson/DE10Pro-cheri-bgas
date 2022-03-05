package CHERI_BGAS_Top;

import DE10Pro_bsv_shell :: *;
import WindCoreInterface :: *;
import AXI4_Fake_16550 :: *;
import Routable :: *;
import SourceSink :: *;
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
`define F2H_ADDR    40 // from 20 (1MB) to 40 (1TB)
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

// A straight forward axi lite subordinate to provide a banking mechanism for
// the h2f window into the core's memory map
module mkH2FAddrCtrl #(Bit #(`H2F_LW_DATA) dfltUpperBits)
  (Tuple2 #( AXI4Lite_Slave #( `H2F_LW_ADDR, `H2F_LW_DATA
                             , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                             , `H2F_LW_ARUSER, `H2F_LW_RUSER)
           , ReadOnly #(Bit #(`H2F_LW_DATA)) ));

  // internal state and signals
  let addrUpperBits <- mkReg (dfltUpperBits);
  let axiShim <- mkAXI4LiteShimFF;

  // read requests handling (always answer with upper bits)
  rule read_req;
    axiShim.master.ar.drop;
    axiShim.master.r.put (AXI4Lite_RFlit { rdata: addrUpperBits
                                         , rresp: OKAY
                                         , ruser: ? });
  endrule

  // write requests handling (always update addrUpperBits)
  rule write_req;
    axiShim.master.aw.drop;
    let w <- get (axiShim.master.w);
    addrUpperBits <= w.wdata;
    axiShim.master.b.put (AXI4Lite_BFlit { bresp: OKAY
                                         , buser: ? });
  endrule

  // return the subordinate port and a ReadOnly interface to addrUpperBits
  return tuple2 (axiShim.slave, regToReadOnly (addrUpperBits));

endmodule

// Wrapper around a single CHERI BGAS system
module mkSingleCHERI_BGAS_Top (DE10ProIfc)
  provisos ( NumAlias #(bus_mid_w, TAdd #(Wd_MId, 1)) // id width out of the core
           , NumAlias #(bus_sid_w, TAdd #(Wd_MId, 2)) // cope with 2 masters only
           , Alias #(bus_mngr_t, AXI4_Master #( bus_mid_w, Wd_Addr, Wd_Data
                                              , 0, 0, 0, 0, 0))
           , Alias #(bus_sub_t, AXI4_Slave #( bus_sid_w, Wd_Addr, Wd_Data
                                            , 0, 0, 0, 0, 0))
           , Alias #(bus_subshim_t, AXI4_Shim #( bus_sid_w, Wd_Addr, Wd_Data
                                               , 0, 0, 0, 0, 0))
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
            , ReadOnly #(Bool) ))
    fake16550ifcs <- mkAXI4_Fake_16550_Pair ( 2048
                                            , 2048
                                            , reset_by newRst.new_rst);
  match { {.fake16550s0, .fake16550irq0}
        , {.fake16550s1, .fake16550irq1} } = fake16550ifcs;
  // ctrl sub entry
  let ctrSubFake16550 =
        tuple2 (fake16550s0, Range { base: 'h0000_3000, size: 'h0000_1000 });

  // h2f address upper 32-bits banking register
  // (h2f port only has 32-bit addresses, this mechanism is intended to enable
  // control over a full 64-bit address)
  Tuple2 #( AXI4Lite_Slave #( `H2F_LW_ADDR, `H2F_LW_DATA
                            , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                            , `H2F_LW_ARUSER, `H2F_LW_RUSER)
          , ReadOnly #(Bit #(`H2F_LW_DATA)) )
    h2fCtrlIfcs <- mkH2FAddrCtrl (0, reset_by newRst.new_rst);
  match {.h2fAddrCtrlSub, .h2fAddrCtrlRO} = h2fCtrlIfcs;
  // ctrl sub entry
  let ctrSubH2FAddrCtrl =
        tuple2 (h2fAddrCtrlSub, Range { base: 'h0000_4000, size: 'h0000_1000 });

  // prepare AXI4 managers
  //////////////////////////////////////////////////////////////////////////////

  // convert to WindCoreHi interface and map additional AXI4 Lite subordinates
  let core <- windCoreMid2Hi_Core (
                midCore
                // This subordinate is facing the outside world
              , cons (ctrSubFake16550, cons (ctrSubH2FAddrCtrl, nil))
                // This irq is going into the RISCV core
              , cons (fake16550irq1, nil)
              , reset_by newRst.new_rst );

  // gather all managers
  Vector #(2, bus_mngr_t) ms;
  ms[0] = core.manager_0;
  ms[1] = core.manager_1;

  // prepare AXI4 subordinates
  //////////////////////////////////////////////////////////////////////////////

  // prepare f2h interface
  bus_subshim_t f2hShim <- mkAXI4ShimFF (reset_by newRst.new_rst);
  // wid and arid reallocation
  let f2hTmpIfc <-
    sizedSerializeWithId_AXI4_Master ( 4, 0, 4, 0, f2hShim.master
                                     , reset_by newRst.new_rst );
  AXI4_Master #( `F2H_ID, `F2H_ADDR, `F2H_DATA
               , `F2H_AWUSER, `F2H_WUSER, `F2H_BUSER
               , `F2H_ARUSER, `F2H_RUSER)
    f2hMngrIfc <- toWider_AXI4_Master ( truncate_AXI4_Master_addr (f2hTmpIfc)
                                      , reset_by newRst.new_rst );

  // prepare fake 16550
  AXI4_Shim #( bus_sid_w, `H2F_LW_ADDR, `H2F_LW_DATA
             , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
             , `H2F_LW_ARUSER, `H2F_LW_RUSER)
    fake16550DeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  mkConnection (fake16550DeBurst.master, fake16550s1, reset_by newRst.new_rst);
  bus_sub_t fake16550_s <-
    toWider_AXI4_Slave ( truncate_AXI4_Slave_addr (fake16550DeBurst.slave)
                       , reset_by newRst.new_rst );

  // prepare bootrom
  bus_subshim_t fakeBootRomDeBurst <-
    mkBurstToNoBurst (reset_by newRst.new_rst);
  bus_sub_t fakeBootRom <- mkPerpetualZeroAXI4Slave (reset_by newRst.new_rst);
  mkConnection ( fakeBootRomDeBurst.master, fakeBootRom
               , reset_by newRst.new_rst);

  // prepare ddr channel
  bus_subshim_t ddrDeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  let ddr_mngr <-
    toWider_AXI4_Master ( truncate_AXI4_Master_addr (ddrDeBurst.master)
                        , reset_by newRst.new_rst );

  // gather all subordinates
  Vector #(4, bus_sub_t) ss;
  ss[0] = ddrDeBurst.slave;
  ss[1] = fake16550_s;
  ss[2] = debugAXI4_Slave (fakeBootRomDeBurst.slave, $format ("fake bootRom"));
  ss[3] = f2hShim.slave;

  // build route
  SoC_Map_IFC soc_map <- mkSoC_Map (reset_by newRst.new_rst);
  function Vector #(4, Bool) route (Bit #(Wd_Addr) addr);
    Vector #(4, Bool) x = unpack (4'b0000);
    if (inRange (soc_map.m_f2h_addr_range, addr))
      x[3] = True;
    else if (inRange (soc_map.m_boot_rom_addr_range, addr))
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

  Vector #(32, Irq) allIrqs = replicate (noIrq);
  // the fake 16550 irq
  allIrqs[0] = interface Irq; method _read = fake16550irq0._read; endinterface;

  // prepare h2f subordinate interface
  let h2fSub <-
    toWider_AXI4_Slave ( zero_AXI4_Slave_user (
                           prepend_AXI4_Slave_addr ( h2fAddrCtrlRO
                                                   , core.subordinate_0 ))
                       , reset_by newRst.new_rst );

  // interface
  interface axls_h2f_lw = core.control_subordinate;
  interface axs_h2f = h2fSub;
  interface axm_f2h = f2hMngrIfc;
  interface axm_ddrb = ddr_mngr;
  interface axm_ddrc = culDeSac;
  interface axm_ddrd = culDeSac;
  interface irqs = allIrqs;

endmodule

`ifdef NB_CHERI_BGAS_SYSTEMS
typedef `NB_CHERI_BGAS_SYSTEMS NBCheriBgasSystems;
`else
typedef 1 NBCheriBgasSystems;
`endif
Integer nbCheriBgasSystems = valueOf (NBCheriBgasSystems);

module mkCHERI_BGAS_Top (DE10ProIfc)
  provisos (
    Alias #( ctrlSub_t
           , AXI4Lite_Slave #( `H2F_LW_ADDR, `H2F_LW_DATA
                             , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                             , `H2F_LW_ARUSER, `H2F_LW_RUSER ))
  , Alias #( h2fSub_t
           , AXI4_Slave #( `H2F_ID, `H2F_ADDR, `H2F_DATA
                         , `H2F_AWUSER, `H2F_WUSER, `H2F_BUSER
                         , `H2F_ARUSER, `H2F_RUSER ))
  , Alias #( f2hMngr_t
           , AXI4_Master #( `F2H_ID, `F2H_ADDR, `F2H_DATA
                          , `F2H_AWUSER, `F2H_WUSER, `F2H_BUSER
                          , `F2H_ARUSER, `F2H_RUSER ))
  , Alias #( ddrMngr_t
           , AXI4_Master #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
                          , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
                          , `DRAM_ARUSER, `DRAM_RUSER ))
  , Alias #( irqs_t, Vector #(32, Irq))
  );

  // establish the number of CHERI BGAS systems
  // XXX Only support 2 systems at most for now
  if (nbCheriBgasSystems < 1) error ("nbCheriBgasSystems must be > 0");
  if (nbCheriBgasSystems > 2) error ("nbCheriBgasSystems must be < 3");

  // instantiate CHERI BGAS system(s)
  //////////////////////////////////////////////////////////////////////////////
  Clock clk <- exposeCurrentClock;
  Reset rst <- exposeCurrentReset;
  let newRst <- mkReset (0, True, clk, reset_by rst);
  Vector #(NBCheriBgasSystems, DE10ProIfc)
    sys <- replicateM (mkSingleCHERI_BGAS_Top (reset_by newRst.new_rst));
  // local helper functions
  function ctrlSub_t getH2FLW (DE10ProIfc s) = s.axls_h2f_lw;
  function h2fSub_t getH2F (DE10ProIfc s) = s.axs_h2f;
  function f2hMngr_t getF2H (DE10ProIfc s) = s.axm_f2h;
  function ddrMngr_t getDDRB (DE10ProIfc s) = s.axm_ddrb;
  function irqs_t getIRQs (DE10ProIfc s) = s.irqs;

  // aggregate AXI Lite control traffic
  //////////////////////////////////////////////////////////////////////////////
  // Allocate 16 bits of address space per system, route to a system based on
  // addr[17:16]: 2'b00 -> system 0
  //              2'b01 -> system 1
  //              2'b10 -> system 2
  //              2'b11 -> h2f system selector (device to select which system is
  //                       accessed upon h2f device reads/writes)

  // AXI lite shim
  AXI4Lite_Shim #( `H2F_LW_ADDR, `H2F_LW_DATA
                 , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                 , `H2F_LW_ARUSER, `H2F_LW_RUSER )
    h2flwShim <- mkAXI4LiteShimFF (reset_by newRst.new_rst);
  // actual subordinates
  Vector #(4, ctrlSub_t) h2flwSubs = replicate (culDeSac);
  for (Integer i = 0; i < nbCheriBgasSystems; i = i + 1)
    h2flwSubs[i] = mask_AXI4Lite_Slave_addr ( zeroExtend (16'hffff)
                                            , getH2FLW (sys[i]) );
  // assign h2f system selector device
  h2flwSubs[3] = culDeSac; // XXX TODO
  // control traffic routing function
  function route_lw (addr);
    Vector #(4, Bool) res = replicate (False);
    res[addr[17:16]] = True;
    return res;
  endfunction
  // wire up
  mkAXI4LiteBus ( route_lw, cons (h2flwShim.master, nil), h2flwSubs
                , reset_by newRst.new_rst );

  // aggregate AXI h2f traffic
  //////////////////////////////////////////////////////////////////////////////
  AXI4_Shim #( `H2F_ID, `H2F_ADDR, `H2F_DATA
             , `H2F_AWUSER, `H2F_WUSER, `H2F_BUSER
             , `H2F_ARUSER, `H2F_RUSER )
    h2fShim <- mkAXI4ShimFF (reset_by newRst.new_rst);
  Vector #(NBCheriBgasSystems, h2fSub_t) h2fSubs = map (getH2F, sys);

  // XXX TODO use the system selector device to route
  mkAXI4Bus ( constFn (unpack ('b1)), cons (h2fShim.master, nil), h2fSubs
            , reset_by newRst.new_rst );

  // aggregate AXI f2h traffic
  //////////////////////////////////////////////////////////////////////////////
  AXI4_Shim #( `F2H_ID, `F2H_ADDR, `F2H_DATA
             , `F2H_AWUSER, `F2H_WUSER, `F2H_BUSER
             , `F2H_ARUSER, `F2H_RUSER )
    f2hShim <- mkAXI4ShimFF (reset_by newRst.new_rst);
  // XXX TODO wire up all systems
  //Vector #(NBCheriBgasSystems, f2hMngr_t) f2hMngrs = map (getF2H, sys);
  //mkAXI4Bus ( constFn (cons (True, nil)), f2hMngrs, cons (f2hShim.slave, nil)
  //          , reset_by newRst.new_rst );
  mkConnection (getF2H (sys[0]), f2hShim.slave, reset_by newRst.new_rst);

  // dispatch ddr channels
  //////////////////////////////////////////////////////////////////////////////
  Vector #(3, ddrMngr_t) ddr = replicate (culDeSac);
  for (Integer i = 0; i < nbCheriBgasSystems; i = i + 1)
    ddr[i] = getDDRB (sys[i]);

  // dispatch IRQs
  //////////////////////////////////////////////////////////////////////////////
  irqs_t allIrqs = replicate (noIrq);
  // allocate 8 IRQ lines per system
  for (Integer i = 0; i < nbCheriBgasSystems; i = i + 1) begin
    Integer offset = i * 8;
    for (Integer j = 0; j < 8; j = j + 1)
      allIrqs[offset + j] = asIfc (getIRQs (sys[i])[j]);
  end

  // interface
  //////////////////////////////////////////////////////////////////////////////
  interface axls_h2f_lw = h2flwShim.slave;
  interface axs_h2f = h2fShim.slave;
  interface axm_f2h = f2hShim.master;
  // XXX
  //interface axm_ddrb = ddr[0];
  //interface axm_ddrc = ddr[1];
  //interface axm_ddrd = ddr[2];
  // XXX Only support 2 systems at most for now, and force system 2 to use ddrd
  //     due to a currently unresolved quartus fitter issue
  interface axm_ddrb = ddr[0];
  interface axm_ddrc = culDeSac;
  interface axm_ddrd = ddr[1];
  // XXX
  interface irqs = allIrqs;
endmodule

(* synthesize *)
module mkCHERI_BGAS_Top_Sig (DE10ProIfcSig);
  let noSigIfc <- mkCHERI_BGAS_Top;
  let sigIfc <- toDE10Pro_bsv_shell_Sig (noSigIfc);
  return sigIfc;
endmodule

endpackage
