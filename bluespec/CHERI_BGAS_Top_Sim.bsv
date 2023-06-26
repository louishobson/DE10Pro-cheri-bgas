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

package CHERI_BGAS_Top_Sim;

import FIFOF :: *;
import BlueBasics :: *;
import BlueAXI4 :: *;
import BlueAvalon :: *;
import BlueUnixBridges :: *;
import BlueUtils :: *;
import Connectable :: *;
import CHERI_BGAS_Top :: *;
import DE10Pro_bsv_shell :: *;

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
`define F2H_ADDR    38 // from 20 (1MB) to 40 (1TB)
`define F2H_DATA   128
`define F2H_AWUSER   0
`define F2H_WUSER    0
`define F2H_BUSER    0
`define F2H_ARUSER   0
`define F2H_RUSER    0

// DDR AXI ports parameters

`define DRAM_ID       8
`define DRAM_ADDR    32
`define DRAM_DATA   512
`define DRAM_AWUSER   0
`define DRAM_WUSER    0
`define DRAM_BUSER    0
`define DRAM_ARUSER   0
`define DRAM_RUSER    0

// Simulation toplevel module
////////////////////////////////////////////////////////////////////////////////

module mkCHERI_BGAS_Top_Sim (Empty);

  // topmodule to simulate
  DE10ProIfc cheri_bgas_top <- mkCHERI_BGAS_Top;

  // unix FIFOs for the router ports
  Sink #(Bit #(512))
    northTX <- mkUnixFifoSink ("simports/bgas-global-ports/north/tx");
  mkConnection (cheri_bgas_top.tx_north, northTX);
  Sink #(Bit #(512))
    eastTX <- mkUnixFifoSink ("simports/bgas-global-ports/east/tx");
  mkConnection (cheri_bgas_top.tx_east, eastTX);
  Sink #(Bit #(512))
    southTX <- mkUnixFifoSink ("simports/bgas-global-ports/south/tx");
  mkConnection (cheri_bgas_top.tx_south, southTX);
  Sink #(Bit #(512))
    westTX <- mkUnixFifoSink ("simports/bgas-global-ports/west/tx");
  mkConnection (cheri_bgas_top.tx_west, westTX);
  Source #(Bit #(512))
    northRX <- mkUnixFifoSource ("simports/bgas-global-ports/north/rx");
  mkConnection (cheri_bgas_top.rx_north, northRX);
  Source #(Bit #(512))
    eastRX <- mkUnixFifoSource ("simports/bgas-global-ports/east/rx");
  mkConnection (cheri_bgas_top.rx_east, eastRX);
  Source #(Bit #(512))
    southRX <- mkUnixFifoSource ("simports/bgas-global-ports/south/rx");
  mkConnection (cheri_bgas_top.rx_south, southRX);
  Source #(Bit #(512))
    westRX <- mkUnixFifoSource ("simports/bgas-global-ports/west/rx");
  mkConnection (cheri_bgas_top.rx_west, westRX);

  // H2F_LW port
  AXI4_Master #( 0, `H2F_LW_ADDR, `H2F_LW_DATA
               , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
               , `H2F_LW_ARUSER, `H2F_LW_RUSER )
    h2f_lw_mngr <- mkUnixFifo_AXI4_Master ("simports/h2f_lw");
  mkConnection ( debugAXI4_Master (h2f_lw_mngr, $format ("h2f_lw_mngr"))
               , cheri_bgas_top.axls_h2f_lw );

  // H2F port
  AXI4_Master #( `H2F_ID, `H2F_ADDR, `H2F_DATA
               , `H2F_AWUSER, `H2F_WUSER, `H2F_BUSER
               , `H2F_ARUSER, `H2F_RUSER )
    h2f_mngr <- mkUnixFifo_AXI4_Master ("simports/h2f");
  mkConnection ( debugAXI4_Master (h2f_mngr, $format ("h2f_mngr"))
               , cheri_bgas_top.axs_h2f );

  // F2H port
  AXI4_Slave #( `F2H_ID, `F2H_ADDR, `F2H_DATA
              , `F2H_AWUSER, `F2H_WUSER, `F2H_BUSER
              , `F2H_ARUSER, `F2H_RUSER )
    f2h_sub <- mkUnixFifo_AXI4_Slave ("simports/f2h");
  mkConnection ( cheri_bgas_top.axm_f2h
               , debugAXI4_Slave (f2h_sub, $format ("f2h_sub")) );
  // DDR B channel
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRB <- mkAXI4Mem ( 1073741824
                          , FilePathEnvVar ("CHERI_BGAS_DDRB_HEX_INIT") );
  mkConnection ( cheri_bgas_top.axm_ddrb
               , debugAXI4_Slave (fakeDDRB, $format ("ddrb")));
               //, fakeDDRB );
  // DDR C channel
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRC <- mkAXI4Mem (4096, UnInit);
  mkConnection ( cheri_bgas_top.axm_ddrc
               , debugAXI4_Slave (fakeDDRC, $format ("ddrc")));
               //, fakeDDRC );
  // DDR D channel
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRD <- mkAXI4Mem (4096, UnInit);
  mkConnection ( cheri_bgas_top.axm_ddrd
               , debugAXI4_Slave (fakeDDRD, $format ("ddrd")));
               //, fakeDDRD );
endmodule

module mkCHERI_BGAS_Top_Sim_AvalonDDR (Empty);

  // topmodule to simulate
  DE10ProIfcSigAvalon cheri_bgas_top <- mkCHERI_BGAS_Top_Sig_Avalon;

  // unix FIFOs for the router ports
  Sink #(Bit #(512))
    northTX <- mkUnixFifoSink ("simports/bgas-global-ports/north/tx");
  mkConnection (cheri_bgas_top.tx_north, northTX);
  Sink #(Bit #(512))
    eastTX <- mkUnixFifoSink ("simports/bgas-global-ports/east/tx");
  mkConnection (cheri_bgas_top.tx_east, eastTX);
  Sink #(Bit #(512))
    southTX <- mkUnixFifoSink ("simports/bgas-global-ports/south/tx");
  mkConnection (cheri_bgas_top.tx_south, southTX);
  Sink #(Bit #(512))
    westTX <- mkUnixFifoSink ("simports/bgas-global-ports/west/tx");
  mkConnection (cheri_bgas_top.tx_west, westTX);
  Source #(Bit #(512))
    northRX <- mkUnixFifoSource ("simports/bgas-global-ports/north/rx");
  mkConnection (cheri_bgas_top.rx_north, northRX);
  Source #(Bit #(512))
    eastRX <- mkUnixFifoSource ("simports/bgas-global-ports/east/rx");
  mkConnection (cheri_bgas_top.rx_east, eastRX);
  Source #(Bit #(512))
    southRX <- mkUnixFifoSource ("simports/bgas-global-ports/south/rx");
  mkConnection (cheri_bgas_top.rx_south, southRX);
  Source #(Bit #(512))
    westRX <- mkUnixFifoSource ("simports/bgas-global-ports/west/rx");
  mkConnection (cheri_bgas_top.rx_west, westRX);

  // H2F_LW port
  AXI4_Master #( 0, `H2F_LW_ADDR, `H2F_LW_DATA
               , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
               , `H2F_LW_ARUSER, `H2F_LW_RUSER )
    h2f_lw_mngr <- mkUnixFifo_AXI4_Master ("simports/h2f_lw");
  let axls_h2f_lw <- fromAXI4Lite_Slave_Sig (cheri_bgas_top.axls_h2f_lw);
  mkConnection ( debugAXI4_Master (h2f_lw_mngr, $format ("h2f_lw_mngr"))
               , axls_h2f_lw );

  // H2F port
  AXI4_Master #( `H2F_ID, `H2F_ADDR, `H2F_DATA
               , `H2F_AWUSER, `H2F_WUSER, `H2F_BUSER
               , `H2F_ARUSER, `H2F_RUSER )
    h2f_mngr <- mkUnixFifo_AXI4_Master ("simports/h2f");
  let axs_h2f <- fromAXI4_Slave_Sig (cheri_bgas_top.axs_h2f);
  mkConnection ( debugAXI4_Master (h2f_mngr, $format ("h2f_mngr"))
               , axs_h2f );

  // F2H port
  AXI4_Slave #( `F2H_ID, `F2H_ADDR, `F2H_DATA
              , `F2H_AWUSER, `F2H_WUSER, `F2H_BUSER
              , `F2H_ARUSER, `F2H_RUSER )
    f2h_sub <- mkUnixFifo_AXI4_Slave ("simports/f2h");
  let axm_f2h <- fromAXI4_Master_Sig (cheri_bgas_top.axm_f2h);
  mkConnection ( axm_f2h
               , debugAXI4_Slave (f2h_sub, $format ("f2h_sub")) );
  NumProxy #(4) depth_proxy = error ("Don't look inside a proxy");
  // DDR B channel
  Slave #( AvalonMMRequest #(`DRAM_ADDR, `DRAM_DATA)
         , AvalonMMResponse #(`DRAM_DATA) )
    fakeDDRB <- mkAvalonMMMem ( 1073741824
                              , FilePathEnvVar ("CHERI_BGAS_DDRB_HEX_INIT") );
  Tuple3 #( Source #(AvalonMMRequest #(`DRAM_ADDR, `DRAM_DATA))
          , Sink #(AvalonMMResponse #(`DRAM_DATA))
          , PipelinedAvalonMMAgent #(`DRAM_ADDR, `DRAM_DATA) )
    {reqB, rspB, agentB} <- toPipelinedAvalonMMAgent (depth_proxy);
  mkConnection (cheri_bgas_top.avm_ddrb, agentB);
  mkConnection (tuple2 (reqB, rspB), debugSlave (fakeDDRB, $format ("ddrb")));
               //, fakeDDRB );
  // DDR C channel
  Slave #( AvalonMMRequest #(`DRAM_ADDR, `DRAM_DATA)
         , AvalonMMResponse #(`DRAM_DATA) )
    fakeDDRC <- mkAvalonMMMem (4096, UnInit);
  Tuple3 #( Source #(AvalonMMRequest #(`DRAM_ADDR, `DRAM_DATA))
          , Sink #(AvalonMMResponse #(`DRAM_DATA))
          , PipelinedAvalonMMAgent #(`DRAM_ADDR, `DRAM_DATA) )
    {reqC, rspC, agentC} <- toPipelinedAvalonMMAgent (depth_proxy);
  mkConnection (cheri_bgas_top.avm_ddrc, agentC);
  mkConnection (tuple2 (reqC, rspC), debugSlave (fakeDDRC, $format ("ddrc")));
               //, fakeDDRC );
  // DDR D channel
  Slave #( AvalonMMRequest #(`DRAM_ADDR, `DRAM_DATA)
         , AvalonMMResponse #(`DRAM_DATA) )
    fakeDDRD <- mkAvalonMMMem (4096, UnInit);
  Tuple3 #( Source #(AvalonMMRequest #(`DRAM_ADDR, `DRAM_DATA))
          , Sink #(AvalonMMResponse #(`DRAM_DATA))
          , PipelinedAvalonMMAgent #(`DRAM_ADDR, `DRAM_DATA) )
    {reqD, rspD, agentD} <- toPipelinedAvalonMMAgent (depth_proxy);
  mkConnection (cheri_bgas_top.avm_ddrd, agentD);
  mkConnection (tuple2 (reqD, rspD), debugSlave (fakeDDRD, $format ("ddrd")));
               //, fakeDDRD );
endmodule

endpackage
