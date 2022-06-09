/*-
 * Copyright (c) 2021-2022 Alexandre Joannou
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

package CHERI_BGAS_Top;

import DE10Pro_bsv_shell :: *;
import CHERI_BGAS_System :: *;
import AXI4 :: *;
import AXI4Lite :: *;
import AXI4Stream :: *;
import AXI4_AXI4Lite_Bridges :: *;
//import BlueBasics :: *;
import Primitives :: *;
import Stratix10ChipID :: *;
import Vector :: *;
import Clocks :: *;
import Connectable :: *;

import CHERI_BGAS_System :: *;
//import CHERI_BGAS_Bridge :: *;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

`ifdef NB_CHERI_BGAS_SYSTEMS
typedef `NB_CHERI_BGAS_SYSTEMS NBCheriBgasSystems;
`else
typedef 1 NBCheriBgasSystems;
`endif
Integer nbCheriBgasSystems = valueOf (NBCheriBgasSystems);

// Concrete parameters definitions
// -------------------------------

`define H2F_LW_ADDR   21 // from 20 (1MB) to 21 (2MB)
`define H2F_LW_DATA   32
`define H2F_LW_AWUSER  0
`define H2F_LW_WUSER   0
`define H2F_LW_BUSER   0
`define H2F_LW_ARUSER  0
`define H2F_LW_RUSER   0

`define H2F_ID       3
`define H2F_ADDR    32 // from 20 (1MB) to 32 (4GB)
`define H2F_DATA   128 // 32, 64 or 128
`define H2F_AWUSER   0
`define H2F_WUSER    0
`define H2F_BUSER    0
`define H2F_ARUSER   0
`define H2F_RUSER    0

//`define F2H_ID       4
`define F2H_ID       9
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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

module mkCHERI_BGAS_Top (DE10ProIfc)
provisos (
  // Numeric type aliases
  //////////////////////////////////////////////////////////////////////////////
  // Numeric aliases for inner CHERI-BGAS system(s)
  // AXI4Lite subordinate port - incoming control traffic
  NumAlias #(t_sys_axil_sub_addr, `H2F_LW_ADDR)
, NumAlias #(t_sys_axil_sub_data, `H2F_LW_DATA)
, NumAlias #(t_sys_axil_sub_awuser, `H2F_LW_AWUSER)
, NumAlias #(t_sys_axil_sub_wuser, `H2F_LW_WUSER)
, NumAlias #(t_sys_axil_sub_buser, `H2F_LW_BUSER)
, NumAlias #(t_sys_axil_sub_aruser, `H2F_LW_ARUSER)
, NumAlias #(t_sys_axil_sub_ruser, `H2F_LW_RUSER)
  // AXI4 subordinate port 0 - incoming H2F traffic
, NumAlias #(t_sys_axi_sub_0_id, `H2F_ID)
, NumAlias #(t_sys_axi_sub_0_addr, `H2F_ADDR)
, NumAlias #(t_sys_axi_sub_0_data, `H2F_DATA)
, NumAlias #(t_sys_axi_sub_0_awuser, `H2F_AWUSER)
, NumAlias #(t_sys_axi_sub_0_wuser, `H2F_WUSER)
, NumAlias #(t_sys_axi_sub_0_buser, `H2F_BUSER)
, NumAlias #(t_sys_axi_sub_0_aruser, `H2F_ARUSER)
, NumAlias #(t_sys_axi_sub_0_ruser, `H2F_RUSER)
  // AXI4 subordinate port 1 - incoming global traffic
, NumAlias #(t_sys_axi_sub_1_id, 3)
, NumAlias #(t_sys_axi_sub_1_addr, 64)
, NumAlias #(t_sys_axi_sub_1_data, 64)
, NumAlias #(t_sys_axi_sub_1_awuser, 0)
, NumAlias #(t_sys_axi_sub_1_wuser, 1)
, NumAlias #(t_sys_axi_sub_1_buser, 0)
, NumAlias #(t_sys_axi_sub_1_aruser, 0)
, NumAlias #(t_sys_axi_sub_1_ruser, 1)
  // AXI4 manager ports - outgoing F2H, DDR and global traffic
, NumAlias #(t_sys_axi_mngr_id, 8)
, NumAlias #(t_sys_axi_mngr_addr, 64)
, NumAlias #(t_sys_axi_mngr_data, 64)
, NumAlias #(t_sys_axi_mngr_awuser, 0)
, NumAlias #(t_sys_axi_mngr_wuser, 0)
, NumAlias #(t_sys_axi_mngr_buser, 0)
, NumAlias #(t_sys_axi_mngr_aruser, 0)
, NumAlias #(t_sys_axi_mngr_ruser, 0)
  //////////////////////////////////////////////////////////////////////////////
  // Numeric aliases for the CHERI-BGAS toplevel module
  // node ID
, NumAlias #(t_node_id_sz, 16)
  // AXI4 global traffic ports
, NumAlias #(t_global_axi_id, TAdd #(t_sys_axi_mngr_id, t_node_id_sz))
, NumAlias #(t_global_axi_addr, 64)
, NumAlias #(t_global_axi_data, 64)
, NumAlias #(t_global_axi_awuser, 0)
, NumAlias #(t_global_axi_wuser, 1)
, NumAlias #(t_global_axi_buser, 0)
, NumAlias #(t_global_axi_aruser, 0)
, NumAlias #(t_global_axi_ruser, 1)
  // Interrupts
, Alias #( t_irqs, Vector #(32, Irq))
  // global aggregated traffic streams
, Alias #( t_bgas_streams
         , Tuple2 #( AXI4Stream_Master #(0, 512, 0, 0)
                   , AXI4Stream_Slave #(0, 512, 0, 0)))
  //////////////////////////////////////////////////////////////////////////////
  // local CHERI-BGAS system types
, Alias #( t_cheri_bgas_sys, CHERI_BGAS_System_Ifc # (
    // AXI4Lite subordinate port - incoming control traffic
      t_sys_axil_sub_addr, t_sys_axil_sub_data
    , t_sys_axil_sub_awuser, t_sys_axil_sub_wuser, t_sys_axil_sub_buser
    , t_sys_axil_sub_aruser, t_sys_axil_sub_ruser
    // AXI4 subordinate 0 port - incoming H2F traffic
    , t_sys_axi_sub_0_id, t_sys_axi_sub_0_addr, t_sys_axi_sub_0_data
    , t_sys_axi_sub_0_awuser, t_sys_axi_sub_0_wuser, t_sys_axi_sub_0_buser
    , t_sys_axi_sub_0_aruser, t_sys_axi_sub_0_ruser
    // AXI4 subordinate 1 port - incoming global traffic
    , t_sys_axi_sub_1_id, t_sys_axi_sub_1_addr, t_sys_axi_sub_1_data
    , t_sys_axi_sub_1_awuser, t_sys_axi_sub_1_wuser, t_sys_axi_sub_1_buser
    , t_sys_axi_sub_1_aruser, t_sys_axi_sub_1_ruser
    // AXI4 manager ports
    , t_sys_axi_mngr_id, t_sys_axi_mngr_addr, t_sys_axi_mngr_data
    , t_sys_axi_mngr_awuser, t_sys_axi_mngr_wuser, t_sys_axi_mngr_buser
    , t_sys_axi_mngr_aruser, t_sys_axi_mngr_ruser ) )
  //////////////////////////////////////////////////////////////////////////////
, Alias #( t_sys_axi_sub_0, AXI4_Slave #(
      t_sys_axi_sub_0_id, t_sys_axi_sub_0_addr, t_sys_axi_sub_0_data
    , t_sys_axi_sub_0_awuser, t_sys_axi_sub_0_wuser, t_sys_axi_sub_0_buser
    , t_sys_axi_sub_0_aruser, t_sys_axi_sub_0_ruser ))
, Alias #( t_sys_axi_sub_1, AXI4_Slave #(
      t_sys_axi_sub_1_id, t_sys_axi_sub_1_addr, t_sys_axi_sub_1_data
    , t_sys_axi_sub_1_awuser, t_sys_axi_sub_1_wuser, t_sys_axi_sub_1_buser
    , t_sys_axi_sub_1_aruser, t_sys_axi_sub_1_ruser ))
, Alias #( t_sys_axi_sub_1_mngr, AXI4_Master #(
      t_sys_axi_sub_1_id, t_sys_axi_sub_1_addr, t_sys_axi_sub_1_data
    , t_sys_axi_sub_1_awuser, t_sys_axi_sub_1_wuser, t_sys_axi_sub_1_buser
    , t_sys_axi_sub_1_aruser, t_sys_axi_sub_1_ruser ))
, Alias #( t_sys_axi_mngr, AXI4_Master #(
      t_sys_axi_mngr_id, t_sys_axi_mngr_addr, t_sys_axi_mngr_data
    , t_sys_axi_mngr_awuser, t_sys_axi_mngr_wuser, t_sys_axi_mngr_buser
    , t_sys_axi_mngr_aruser, t_sys_axi_mngr_ruser ))
, Alias #( t_global_mngr, AXI4_Master #(
      t_global_axi_id, t_global_axi_addr, t_global_axi_data
    , t_global_axi_awuser, t_global_axi_wuser, t_global_axi_buser
    , t_global_axi_aruser, t_global_axi_ruser ))
, Alias #( t_global_sub, AXI4_Slave #(
      t_global_axi_id, t_global_axi_addr, t_global_axi_data
    , t_global_axi_awuser, t_global_axi_wuser, t_global_axi_buser
    , t_global_axi_aruser, t_global_axi_ruser ))
, Alias #( t_ctrl_sub
         , AXI4Lite_Slave #( `H2F_LW_ADDR, `H2F_LW_DATA
                           , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                           , `H2F_LW_ARUSER, `H2F_LW_RUSER ))
, Alias #( t_h2f_sub
         , AXI4_Slave #( `H2F_ID, `H2F_ADDR, `H2F_DATA
                       , `H2F_AWUSER, `H2F_WUSER, `H2F_BUSER
                       , `H2F_ARUSER, `H2F_RUSER ))
, Alias #( t_f2h_mngr
         , AXI4_Master #( t_sys_axi_mngr_id, `F2H_ADDR, `F2H_DATA
                        , `F2H_AWUSER, `F2H_WUSER, `F2H_BUSER
                        , `F2H_ARUSER, `F2H_RUSER ))
, Alias #( t_ddr_mngr
         , AXI4_Master #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
                        , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
                        , `DRAM_ARUSER, `DRAM_RUSER ))
);

  // establish the number of CHERI BGAS systems
  // XXX Only support 2 systems at most for now
  if (nbCheriBgasSystems < 1) error ("nbCheriBgasSystems must be > 0");
  if (nbCheriBgasSystems > 2) error ("nbCheriBgasSystems must be < 3");

  // instantiate CHERI BGAS system(s) and global router
  //////////////////////////////////////////////////////////////////////////////
  // instantiate the systems themselves
  Clock clk <- exposeCurrentClock;
  Reset rst <- exposeCurrentReset;
  let newRst <- mkReset (0, True, clk, reset_by rst);
  Vector #(NBCheriBgasSystems, t_cheri_bgas_sys)
    sys <- replicateM (mkCHERI_BGAS_System (reset_by newRst.new_rst));
  Vector #(NBCheriBgasSystems, t_global_mngr)
    globalMngr = replicate (?);
  Vector #(NBCheriBgasSystems, t_global_sub)
    globalSub = replicate (?);

  // local helper functions
  function t_ctrl_sub getH2FLWSub (t_cheri_bgas_sys ifc) = ifc.axil_sub;
  function t_sys_axi_sub_0 getH2FSub (t_cheri_bgas_sys ifc) = ifc.axi_sub_0;
  function t_sys_axi_sub_1 getGlobalSub (t_cheri_bgas_sys ifc) = ifc.axi_sub_1;
  function t_sys_axi_mngr getF2HMngr (t_cheri_bgas_sys ifc) = ifc.axi_mngr_0;
  function t_sys_axi_mngr getDDRMngr (t_cheri_bgas_sys ifc) = ifc.axi_mngr_1;
  function t_sys_axi_mngr getGlobalMngr (t_cheri_bgas_sys ifc) = ifc.axi_mngr_2;
  function t_irqs getIRQs (t_cheri_bgas_sys s) = s.irqs;

  // global network connections
  //////////////////////////////////////////////////////////////////////////////
  // XXX The global Network can support capability tags in its AXI user fields.
  // XXX It is not possible to "speak capability" past the capability tag
  // XXX controller. It should eventually be moved nearer the ddr and the user
  // XXX field should be exported.
  // create the outgoing local -> global ID conversion
  // (simple concat of unique id for now)
  Bit #(t_node_id_sz) nodeId = ?; // TODO
  for (Integer i = 0; i < nbCheriBgasSystems; i = i + 1)
    globalMngr[i] =
      prepend_AXI4_Master_id ( nodeId
                             , zero_AXI4_Master_user (getGlobalMngr (sys[i])) );
  // create the incoming global -> local ID conversion
  // (ID realocation)
  NumProxy #(16) proxyTableSz = ?;
  NumProxy #(8)  proxyMaxSameId = ?;
  for (Integer i = 0; i < nbCheriBgasSystems; i = i + 1) begin
    Tuple2 #(t_global_sub, t_sys_axi_sub_1_mngr)
      globalTrafficIn <- mkAXI4IDNameSpaceCrossing ( proxyTableSz
                                                   , proxyMaxSameId );
    match {.globalTrafficInSub, .globalTrafficInMngr} = globalTrafficIn;
    mkConnection (globalTrafficInMngr, getGlobalSub (sys[i]));
    globalSub[i] = globalTrafficInSub;
  end

  // for the 2 systems case, connect the CHERI BGAS systems together
  //////////////////////////////////////////////////////////////////////////////
  if (nbCheriBgasSystems == 2) begin
    mkConnection (globalMngr[0], globalSub[1], reset_by rst);
    mkConnection (globalMngr[0], globalSub[1], reset_by rst);
  end

  // aggregate AXI Lite control traffic
  //////////////////////////////////////////////////////////////////////////////
  // Allocate 16 bits of address space per system, route to a system based on
  // addr[17:16]: 2'b00 -> system 0
  //              2'b01 -> system 1
  //              2'b10 -> system 2
  //              2'b11 -> common AXI4 Lite subordinates:
  //                       addr[15]: 1'b0 -> h2f system selector (device to
  //                                         select which system is accessed
  //                                         upon h2f device reads/writes)
  //                                 1'b1 -> Stratix10 ChipID

  // AXI lite shim
  AXI4Lite_Shim #( `H2F_LW_ADDR, `H2F_LW_DATA
                 , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                 , `H2F_LW_ARUSER, `H2F_LW_RUSER )
    h2flwShim <- mkAXI4LiteShimFF (reset_by newRst.new_rst);
  // actual subordinates
  Vector #(5, t_ctrl_sub) h2flwSubs = replicate (culDeSac);
  for (Integer i = 0; i < nbCheriBgasSystems; i = i + 1)
    h2flwSubs[i] = mask_AXI4Lite_Slave_addr ( zeroExtend (16'hffff)
                                            , getH2FLWSub (sys[i]) );
  // assign h2f system selector device
  h2flwSubs[3] = culDeSac; // XXX TODO
  // assign ChipID reader device
  let stratix10ChipID <- mkAXI4_Stratix10ChipID;
  h2flwSubs[4] = stratix10ChipID;
  // control traffic routing function
  function route_lw (addr);
    Vector #(5, Bool) res = replicate (False);
    case (addr[17:16]) matches
      2'b11: if (addr[15] == 1'b0) res[3] = True; else res[4] = True;
      .x: res[x] = True;
    endcase
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
  Vector #(NBCheriBgasSystems, t_sys_axi_sub_0) h2fSubs = map (getH2FSub, sys);

  // XXX TODO use the system selector device to route
  mkAXI4Bus ( constFn (unpack ('b1)), cons (h2fShim.master, nil), h2fSubs
            , reset_by newRst.new_rst );

  // aggregate AXI f2h traffic
  //////////////////////////////////////////////////////////////////////////////
  AXI4_Shim #( `F2H_ID, `F2H_ADDR, `F2H_DATA
             , `F2H_AWUSER, `F2H_WUSER, `F2H_BUSER
             , `F2H_ARUSER, `F2H_RUSER )
    f2hShim <- mkAXI4ShimFF (reset_by newRst.new_rst);
  module prepF2H #(t_cheri_bgas_sys ifc) (t_f2h_mngr);
    let newIfc <- toWider_AXI4_Master (
                    truncate_AXI4_Master_addr (getF2HMngr (ifc)) );
    return newIfc;
  endmodule
  Vector #(NBCheriBgasSystems, t_f2h_mngr) f2hMngrs <- mapM (prepF2H, sys);
  mkAXI4Bus ( constFn (cons (True, nil)), f2hMngrs, cons (f2hShim.slave, nil)
            , reset_by newRst.new_rst );

  // dispatch ddr channels
  //////////////////////////////////////////////////////////////////////////////
  Vector #(3, t_ddr_mngr) ddr = replicate (culDeSac);
  for (Integer i = 0; i < nbCheriBgasSystems; i = i + 1)
    ddr[i] <-
      toWider_AXI4_Master ( truncate_AXI4_Master_addr (getDDRMngr (sys[i]))
                          , reset_by newRst.new_rst );

  // dispatch IRQs
  //////////////////////////////////////////////////////////////////////////////
  t_irqs allIrqs = replicate (noIrq);
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

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

(* synthesize *)
module mkCHERI_BGAS_Top_Sig (DE10ProIfcSig);
  let noSigIfc <- mkCHERI_BGAS_Top;
  let sigIfc <- toDE10Pro_bsv_shell_Sig (noSigIfc);
  return sigIfc;
endmodule

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

endpackage
