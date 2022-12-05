/*-
 * Copyright (c) 2022 Alexandre Joannou
 * All rights reserved.
 *
 * This material is based upon work supported by the DoD Information Analysis
 * Center Program Management Office (DoD IAC PMO), sponsored by the Defense
 * Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
 * opinions, findings and conclusions or recommendations expressed in this
 * material are those of the author(s) and do not necessarily reflect the views
 * of the Air Force Installation Contracting Agency (AFICA).
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package CHERI_BGAS_System;

import Vector :: *;
import Clocks :: *;
import Connectable :: *;
import BlueAXI4 :: *;
import AXI4_Fake_16550 :: *;
import Routable :: *;
import SourceSink :: *;
import Fabric_Defs :: *;
import CoreW :: *;
import WindCoreInterface :: *;
import DE10Pro_bsv_shell :: *;
import SoC_Map :: *;
import VirtualDevice :: *;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// A straight forward axi lite subordinate to provide a banking mechanism for
// the h2f window into the core's memory map
module mkH2FAddrCtrl #(Bit #(t_h2f_lw_data) dfltUpperBits)
  (Tuple2 #( AXI4Lite_Slave #( t_h2f_lw_addr, t_h2f_lw_data
                             , t_h2f_lw_awuser, t_h2f_lw_wuser, t_h2f_lw_buser
                             , t_h2f_lw_aruser, t_h2f_lw_ruser)
           , ReadOnly #(Bit #(t_h2f_lw_data)) ));

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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

interface CHERI_BGAS_System_Ifc #(
// AXI4Lite subordinate port
  numeric type t_axil_sub_addr
, numeric type t_axil_sub_data
, numeric type t_axil_sub_awuser
, numeric type t_axil_sub_wuser
, numeric type t_axil_sub_buser
, numeric type t_axil_sub_aruser
, numeric type t_axil_sub_ruser
// AXI4 subordinate 0 port
, numeric type t_axi_sub_0_id
, numeric type t_axi_sub_0_addr
, numeric type t_axi_sub_0_data
, numeric type t_axi_sub_0_awuser
, numeric type t_axi_sub_0_wuser
, numeric type t_axi_sub_0_buser
, numeric type t_axi_sub_0_aruser
, numeric type t_axi_sub_0_ruser
// AXI4 subordinate 1 port
, numeric type t_axi_sub_1_id
, numeric type t_axi_sub_1_addr
, numeric type t_axi_sub_1_data
, numeric type t_axi_sub_1_awuser
, numeric type t_axi_sub_1_wuser
, numeric type t_axi_sub_1_buser
, numeric type t_axi_sub_1_aruser
, numeric type t_axi_sub_1_ruser
// AXI4 manager ports
, numeric type t_axi_mngr_id
, numeric type t_axi_mngr_addr
, numeric type t_axi_mngr_data
, numeric type t_axi_mngr_awuser
, numeric type t_axi_mngr_wuser
, numeric type t_axi_mngr_buser
, numeric type t_axi_mngr_aruser
, numeric type t_axi_mngr_ruser
);
  // AXI4Lite subordinate port
  // -------------------------
  interface AXI4Lite_Slave #( t_axil_sub_addr
                            , t_axil_sub_data
                            , t_axil_sub_awuser
                            , t_axil_sub_wuser
                            , t_axil_sub_buser
                            , t_axil_sub_aruser
                            , t_axil_sub_ruser ) axil_sub;
  // AXI4 subordinate ports
  // ----------------------
  interface AXI4_Slave #( t_axi_sub_0_id
                        , t_axi_sub_0_addr
                        , t_axi_sub_0_data
                        , t_axi_sub_0_awuser
                        , t_axi_sub_0_wuser
                        , t_axi_sub_0_buser
                        , t_axi_sub_0_aruser
                        , t_axi_sub_0_ruser ) axi_sub_0;
  interface AXI4_Slave #( t_axi_sub_1_id
                        , t_axi_sub_1_addr
                        , t_axi_sub_1_data
                        , t_axi_sub_1_awuser
                        , t_axi_sub_1_wuser
                        , t_axi_sub_1_buser
                        , t_axi_sub_1_aruser
                        , t_axi_sub_1_ruser ) axi_sub_1;
  // AXI4 manager ports
  // ------------------
  interface AXI4_Master #( t_axi_mngr_id
                         , t_axi_mngr_addr
                         , t_axi_mngr_data
                         , t_axi_mngr_awuser
                         , t_axi_mngr_wuser
                         , t_axi_mngr_buser
                         , t_axi_mngr_aruser
                         , t_axi_mngr_ruser ) axi_mngr_0;
  interface AXI4_Master #( TAdd#(t_axi_mngr_id,1)
                         , t_axi_mngr_addr
                         , TMul#(t_axi_mngr_data,8) // 512-bit for DDR interface.
                         , t_axi_mngr_awuser
                         , t_axi_mngr_wuser
                         , t_axi_mngr_buser
                         , t_axi_mngr_aruser
                         , t_axi_mngr_ruser ) axi_mngr_1;
  interface AXI4_Master #( t_axi_mngr_id
                         , t_axi_mngr_addr
                         , t_axi_mngr_data
                         , t_axi_mngr_awuser
                         , t_axi_mngr_wuser
                         , t_axi_mngr_buser
                         , t_axi_mngr_aruser
                         , t_axi_mngr_ruser ) axi_mngr_2;
  // Interrupt sender interface
  // --------------------------
  interface Vector #(32, Irq) irqs;
endinterface

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// Wrapper around a single CHERI BGAS system
module mkCHERI_BGAS_System ( CHERI_BGAS_System_Ifc #(
  // AXI4Lite subordinate port
    t_axil_sub_addr
  , t_axil_sub_data
  , t_axil_sub_awuser
  , t_axil_sub_wuser
  , t_axil_sub_buser
  , t_axil_sub_aruser
  , t_axil_sub_ruser
  // AXI4 subordinate 0 port
  , t_axi_sub_0_id
  , t_axi_sub_0_addr
  , t_axi_sub_0_data
  , t_axi_sub_0_awuser
  , t_axi_sub_0_wuser
  , t_axi_sub_0_buser
  , t_axi_sub_0_aruser
  , t_axi_sub_0_ruser
  // AXI4 subordinate 1 port
  , t_axi_sub_1_id
  , t_axi_sub_1_addr
  , t_axi_sub_1_data
  , t_axi_sub_1_awuser
  , t_axi_sub_1_wuser
  , t_axi_sub_1_buser
  , t_axi_sub_1_aruser
  , t_axi_sub_1_ruser
  // AXI4 manager ports
  , t_axi_mngr_id
  , t_axi_mngr_addr
  , t_axi_mngr_data
  , t_axi_mngr_awuser
  , t_axi_mngr_wuser
  , t_axi_mngr_buser
  , t_axi_mngr_aruser
  , t_axi_mngr_ruser
  )) provisos (
    // type aliases
    ////////////////////////////////////////////////////////////////////////////
    // AXI4 Lite control port
    Alias #( t_axil_sub
           , AXI4Lite_Slave #(
               t_axil_sub_addr, t_axil_sub_data
             , t_axil_sub_awuser, t_axil_sub_wuser, t_axil_sub_buser
             , t_axil_sub_aruser, t_axil_sub_ruser ))
  , Alias #( t_axil_virt_dev_mngr
           , AXI4_Master #(
               t_bus_sid, t_axil_sub_addr, TMul#(t_axil_sub_data,2)
             , t_axil_sub_awuser, t_axil_sub_wuser, t_axil_sub_buser
             , t_axil_sub_aruser, t_axil_sub_ruser ))
  , Alias #( t_axil_full_shim
           , AXI4_Shim #(
               t_bus_sid, t_axil_sub_addr, t_axil_sub_data
             , t_axil_sub_awuser, t_axil_sub_wuser, t_axil_sub_buser
             , t_axil_sub_aruser, t_axil_sub_ruser ))
    // outgoing traffic
  , NumAlias #(t_bus_mid, TAdd #(Wd_MId, 1)) // id width out of the core
  , NumAlias #(t_bus_sid, TAdd #(Wd_MId, 1)) // cope with 1 masters only
  , Alias #(t_bus_mngr, AXI4_Master #( t_bus_mid, Wd_Addr, Wd_Data_Periph
  
                                     , 0, 0, 0, 0, 0))
  , Alias #(t_bus_sub, AXI4_Slave #( t_bus_sid, Wd_Addr, Wd_Data_Periph
                                   , 0, 0, 0, 0, 0))
  , Alias #(t_bus_subshim, AXI4_Shim #( t_bus_sid, Wd_Addr, Wd_Data_Periph
                                      , 0, 0, 0, 0, 0))
  , Alias #(t_ddr_mngr, AXI4_Master #( t_bus_mid, Wd_Addr, Wd_Data
                                     , 0, 0, 0, 0, 0))
  , Alias #(t_ddr_sub, AXI4_Slave #( TAdd#(t_bus_sid, 1), Wd_Addr, Wd_Data
                                   , 0, 0, 0, 0, 0))
  , Alias #(t_ddr_subshim, AXI4_Shim #( TAdd#(t_bus_sid, 1), Wd_Addr, Wd_Data
                                      , 0, 0, 0, 0, 0))
    // incoming traffic
  , Add #(1, t_incoming_id, Wd_CoreW_Bus_MId)
  , Alias #( t_core_sub_shim
           , AXI4_Shim #( t_incoming_id, Wd_Addr, Wd_Data_Periph
                        , 0, 0, 0, 0, 0))
  , Alias #( t_h2f_sub
           , AXI4_Slave #(
               t_incoming_id, t_axi_sub_0_addr, t_axi_sub_0_data
             , t_axi_sub_0_awuser, t_axi_sub_0_wuser, t_axi_sub_0_buser
             , t_axi_sub_0_aruser, t_axi_sub_0_ruser ))
    // type constraints
    ////////////////////////////////////////////////////////////////////////////
    // AXI4Lite subordinate port
  , Add #(0, 21, t_axil_sub_addr) // XXX hard-coded in CoreW_IFC
  , Add #(0, 32, t_axil_sub_data) // XXX hard-coded in CoreW_IFC
  , Add #(0, 0, t_axil_sub_awuser)
  , Add #(0, 0, t_axil_sub_wuser)
  , Add #(0, 0, t_axil_sub_buser)
  , Add #(0, 0, t_axil_sub_aruser)
  , Add #(0, 0, t_axil_sub_ruser)
    // AXI4 subordinate 0 port
  , Add #(0, t_incoming_id, t_axi_sub_0_id)
  , Add #(0, 32, t_axi_sub_0_addr)
  , Add #(0, 128, t_axi_sub_0_data)
    // AXI4 subordinate 1 port
  , Add #(0, t_incoming_id, t_axi_sub_1_id)
  , Add #(0, Wd_Addr, t_axi_sub_1_addr)
  , Add #(0, Wd_Data_Periph, t_axi_sub_1_data)
  , Add #(0, 0, t_axi_sub_1_awuser)
  , Add #(0, 0, t_axi_sub_1_wuser)
  , Add #(0, 0, t_axi_sub_1_buser)
  , Add #(0, 0, t_axi_sub_1_aruser)
  , Add #(0, 0, t_axi_sub_1_ruser)
    // AXI4 manager ports
  , Add #(0, t_bus_sid, t_axi_mngr_id)
  , Add #(0, Wd_Addr, t_axi_mngr_addr)
  , Add #(0, Wd_Data_Periph, t_axi_mngr_data)
  , Add #(0, 0, t_axi_mngr_awuser)
  , Add #(0, 0, t_axi_mngr_wuser)
  , Add #(0, 0, t_axi_mngr_buser)
  , Add #(0, 0, t_axi_mngr_aruser)
  , Add #(0, 0, t_axi_mngr_ruser)
  );

  // declare cpu core with WindCoreMid interface
  //////////////////////////////////////////////////////////////////////////////
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

  // instanciate the SoC_Map
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  SoC_Map_IFC soc_map <- mkSoC_Map (reset_by newRst.new_rst);

  // declare extra AXI4 lite ctrl subordinates
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // uart0 - fake 16550
  Tuple2 #( Tuple2 #(t_axil_sub, ReadOnly #(Bool) )
          , Tuple2 #(t_axil_sub, ReadOnly #(Bool) ))
    uart0ifcs <- mkAXI4_Fake_16550_Pair ( 50_000_000
                                        , 16
                                        , 16
                                        , reset_by newRst.new_rst);
  match { {.uart0s0, .uart0irq0}
        , {.uart0s1, .uart0irq1} } = uart0ifcs;
  // ctrl sub entry
  let ctrSubUART0 =
        tuple2 (uart0s0, Range { base: 'h0000_3000, size: 'h0000_1000 });

  // uart1 - fake 16550
  Tuple2 #( Tuple2 #(t_axil_sub, ReadOnly #(Bool) )
          , Tuple2 #(t_axil_sub, ReadOnly #(Bool) ))
    uart1ifcs <- mkAXI4_Fake_16550_Pair ( 50_000_000
                                        , 2048
                                        , 2048
                                        , reset_by newRst.new_rst);
  match { {.uart1s0, .uart1irq0}
        , {.uart1s1, .uart1irq1} } = uart1ifcs;
  // ctrl sub entry
  let ctrSubUART1 =
        tuple2 (uart1s0, Range { base: 'h0000_4000, size: 'h0000_1000 });

  // h2f address upper 32-bits banking register
  // (h2f port only has 32-bit addresses, this mechanism is intended to enable
  // control over a full 64-bit address)
  Tuple2 #(t_axil_sub, ReadOnly #(Bit #(t_axil_sub_data)))
    h2fCtrlIfcs <- mkH2FAddrCtrl (0, reset_by newRst.new_rst);
  match {.h2fAddrCtrlSub, .h2fAddrCtrlRO} = h2fCtrlIfcs;
  // ctrl sub entry
  let ctrSubH2FAddrCtrl =
    tuple2 (h2fAddrCtrlSub, Range { base: 'h0000_5000, size: 'h0000_1000 });

  // Virtual device for emulating control registers, e.g. for virtio.
  // (Has both a control interface and a virtualised interface;
  // The control interface for AXI4 lite, and virtualised for Toooba MMIO.
  // The virtual device does not support bursts.)
  VirtualDeviceIfc #( t_bus_sid, t_axil_sub_addr, t_axil_sub_data
                    , t_bus_sid, Wd_Addr, Wd_Data_Periph )
    virtDev <- mkVirtualDevice (reset_by newRst.new_rst);
  let ctrSubVirtDevCtrl =
    tuple2 ( fromAXI4ToAXI4Lite_Slave (virtDev.mngt)
           , Range { base: 'h0000_8000, size: 'h0000_4000 } );

  // Outgoing interconnect
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // prepare AXI4 managers
  ////////////////////////
  // re-wrap wind core:
  // - convert mid core to hi core
  // - add outside-world-facing AXI4 Lite subordinates to expose throug the
  //   core's AXI4 Lite subordinate port (with their mappping)
  // - add IRQs into the wind core
  let core <- windCoreMid2Hi_Core (
                // the mid-level interface core to convert
                midCore
                // the vector of additional AXI4 Lite subordinates to expose
              , cons (        ctrSubUART0
                     , cons ( ctrSubUART1
                     , cons ( ctrSubH2FAddrCtrl
                     , cons ( ctrSubVirtDevCtrl
                            , nil ))))
                // the vector of IRQs going in the wind core
              , cons (        uart0irq1
                     , cons ( uart1irq1
                            , nil ))
                // explicit reset_by
              , reset_by newRst.new_rst );

  // gather all managers
  Vector #(1, t_bus_mngr) ms;
  //ms[0] = core.manager_0;
  ms[0] = core.manager_1;

  // prepare AXI4 subordinates exposed to the wind core manager
  /////////////////////////////////////////////////////////////

  // prepare AXI4 manager ports traffic
  Vector #(3, t_bus_subshim)
    mngrShim <- replicateM (mkAXI4ShimFF (reset_by newRst.new_rst));

  // prepare uart0
  AXI4_Shim #( t_bus_sid, t_axil_sub_addr, t_axil_sub_data
             , t_axil_sub_awuser, t_axil_sub_wuser, t_axil_sub_buser
             , t_axil_sub_aruser, t_axil_sub_ruser)
    uart0DeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  mkConnection (uart0DeBurst.master, uart0s1, reset_by newRst.new_rst);
  t_bus_sub uart0_s <-
    toWider_AXI4_Slave ( truncate_AXI4_Slave_addr (uart0DeBurst.slave)
                       , reset_by newRst.new_rst );

  // prepare uart1
  AXI4_Shim #( t_bus_sid, t_axil_sub_addr, t_axil_sub_data
             , t_axil_sub_awuser, t_axil_sub_wuser, t_axil_sub_buser
             , t_axil_sub_aruser, t_axil_sub_ruser)
    uart1DeBurst <- mkBurstToNoBurst (reset_by newRst.new_rst);
  mkConnection (uart1DeBurst.master, uart1s1, reset_by newRst.new_rst);
  t_bus_sub uart1_s <-
    toWider_AXI4_Slave ( truncate_AXI4_Slave_addr (uart1DeBurst.slave)
                       , reset_by newRst.new_rst );

  // prepare bootrom
  t_bus_subshim fakeBootRomDeBurst <-
    mkBurstToNoBurst (reset_by newRst.new_rst);
  t_bus_sub fakeBootRom <- mkPerpetualZeroAXI4Slave (reset_by newRst.new_rst);
  mkConnection ( fakeBootRomDeBurst.master, fakeBootRom
               , reset_by newRst.new_rst);

  // gather all subordinates
  Vector #(7, t_bus_sub) ss = replicate(?);
  //ss[0] = mngrShim[0].slave; // f2h accesses
  ss[1] = mngrShim[1].slave; // ddr accesses
  //ss[2] = mngrShim[2].slave; // global accesses
  ss[3] = uart0_s;
  ss[4] = uart1_s;
  ss[5] = fakeBootRomDeBurst.slave;
  ss[6] = virtDev.virt;

  // build route
  function Vector #(7, Bool) route (Bit #(Wd_Addr) addr);
    Vector #(7, Bool) x = replicate (False);
    if (inRange (soc_map.m_virt_dev_addr_range, addr))
      x[6] = True;
    else if (inRange (soc_map.m_boot_rom_addr_range, addr))
      x[5] = True;
    else if (inRange (soc_map.m_uart_1_addr_range, addr))
      x[4] = True;
    else if (inRange (soc_map.m_uart_0_addr_range, addr))
      x[3] = True;
    /*else if (  inRange (soc_map.m_global_bgas_addr_range, addr)
            || inRange (soc_map.m_bgas_router_conf_addr_range, addr) )
      x[2] = True;*/
    else if (  inRange (soc_map.m_ddr4_0_uncached_addr_range, addr)
            || inRange (soc_map.m_ddr4_0_cached_addr_range, addr) )
      x[1] = True;
    else if (inRange (soc_map.m_f2h_addr_range, addr))
      x[0] = True;
    return x;
  endfunction

  // wire it all up
  mkAXI4Bus (route, ms, ss, reset_by newRst.new_rst);

  // Incoming interconnect
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // gather all managers
  Vector #(2, t_ddr_mngr) dms;
  dms[0] = core.manager_0;
  AXI4_Master #(t_bus_sid, Wd_Addr, TMul#(Wd_Data_Periph,2), 0, 0, 0, 0, 0) m_wide_a <- toWider_AXI4_Master(mngrShim[1].master);
  AXI4_Master #(t_bus_sid, Wd_Addr, TMul#(Wd_Data_Periph,4), 0, 0, 0, 0, 0) m_wide_b <- toWider_AXI4_Master(m_wide_a);
  AXI4_Master #(t_bus_sid, Wd_Addr, Wd_Data,                 0, 0, 0, 0, 0) ddr_uncached_manager <- toWider_AXI4_Master(m_wide_b);
  dms[1] = ddr_uncached_manager;
  t_ddr_subshim ddrShim <- mkAXI4ShimFF (reset_by newRst.new_rst);
  Vector #(1, t_ddr_sub) dss;
  dss[0] = ddrShim.slave;
  
  // build route
  function Vector #(1, Bool) route_ddr (Bit #(Wd_Addr) addr);
    return replicate(True);
  endfunction

  // wire it all up
  mkAXI4Bus (route_ddr, dms, dss, reset_by newRst.new_rst);
  
  // Shims for
  //   - incoming H2F traffic
  //   - incoming global traffic
  Vector #(2, t_core_sub_shim)
    subShim <- replicateM (mkAXI4ShimFF (reset_by newRst.new_rst));

  // H2F interface wrapping (extra address bits & data size shim)
  t_h2f_sub h2fSub <- toWider_AXI4_Slave (
                        zero_AXI4_Slave_user (
                          prepend_AXI4_Slave_addr ( h2fAddrCtrlRO
                                                  , subShim[0].slave))
                                         , reset_by newRst.new_rst );

  mkAXI4Bus ( constFn (cons (True, nil))
            , cons (subShim[0].master, cons (subShim[1].master, nil))
            , cons (core.subordinate_0, nil)
            , reset_by newRst.new_rst );
  
  // DDR interconnect
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // prepare outside-world-facing IRQs
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  Vector #(32, Irq) allIrqs = replicate (noIrq);
  // uart0 irq
  allIrqs[0] = interface Irq; method _read = uart0irq0._read; endinterface;
  // uart1 irq
  allIrqs[1] = interface Irq; method _read = uart1irq0._read; endinterface;

  // interface
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  interface axil_sub = core.control_subordinate; // incoming control traffic
  interface axi_sub_0 = h2fSub;                  // incoming H2F traffic
  interface axi_sub_1 = subShim[1].slave;        // incoming global traffic
  interface axi_mngr_0 = mngrShim[0].master;     // outgoing F2H traffic
  interface axi_mngr_1 = debugAXI4_Master(ddrShim.master, $format("ddr_system"));         // outgoing ddr traffic
  interface axi_mngr_2 = mngrShim[2].master;     // outgoing global traffic
  interface irqs = allIrqs;                      // outgoing interrupts

endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

endpackage
