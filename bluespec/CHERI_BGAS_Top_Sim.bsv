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
import AXI4 :: *;
import AXI4Lite :: *;
import AXI4_AXI4Lite_Bridges :: *;
import BlueUtils :: *;
import SourceSink :: *;
import Connectable :: *;
import CHERI_BGAS_Top :: *;
import DE10Pro_bsv_shell :: *;
import Recipe :: *;
import AXI4_Master_Socket::*;

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

// Simulation toplevel module
////////////////////////////////////////////////////////////////////////////////

module mkCHERI_BGAS_Top_Sim (Empty);

  // topmodule to simulate
  DE10ProIfc cheri_bgas_top <- mkCHERI_BGAS_Top;

  // H2F_LW driver
  AXI4_Master#( 0, `H2F_LW_ADDR, `H2F_LW_DATA
                   , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                   , `H2F_LW_ARUSER, `H2F_LW_RUSER )
    axi_lw_sock <- mkAXI4_Master_Socket(10001);

  // DDRs
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRB <- mkAXI4Mem (268435456, Valid("/tmp/mem.hex"));
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRC <- mkAXI4Mem (4096, Nothing);
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRD <- mkAXI4Mem (4096, Nothing);

  // connect it all up
  mkConnection (cheri_bgas_top.axls_h2f_lw, axi_lw_sock);
  //mkConnection (cheri_bgas_top.axs_h2f, culDeSac);
  //mkConnection (cheri_bgas_top.axm_f2h, culDeSac);
  mkConnection ( cheri_bgas_top.axm_ddrb
               , fakeDDRB);
  mkConnection ( cheri_bgas_top.axm_ddrc
               , fakeDDRC);
  mkConnection ( cheri_bgas_top.axm_ddrd
               , fakeDDRD);
endmodule

endpackage
