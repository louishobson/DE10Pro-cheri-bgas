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

// AXI4Lite control port driver
////////////////////////////////////////////////////////////////////////////////

module mkAXILiteDriver (AXI4Lite_Master #( `H2F_LW_ADDR
                                         , `H2F_LW_DATA
                                         , `H2F_LW_AWUSER
                                         , `H2F_LW_WUSER
                                         , `H2F_LW_BUSER
                                         , `H2F_LW_ARUSER
                                         , `H2F_LW_RUSER ));

  // general helpers
  //////////////////////////////////////////////////////////////////////////////

  let delayReg <- mkRegU;
  function recipeDelay (delay) = rSeq ( rBlock (
    action delayReg <= delay; endaction
    , rWhile ( delayReg > 0, rAct ( action delayReg <= delayReg - 1; endaction))
  ));

  let shim <- mkAXI4LiteShim;

  let readRegRes <- mkRegU; // reg used by reads and writes for return values.
                            // Must be explicitly handled by the caller

  function readReg (verbosity, addr) = rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting readReg, addr = "
                             , $time
                             , fshow (addr))))
    , shim.slave.ar.put (AXI4Lite_ARFlit { araddr: addr
                                         , arprot: 0
                                         , aruser: 0 })
    , action
        let val <- get (shim.slave.r);
        readRegRes <= val.rdata;
      endaction
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended readReg, value returned = "
                             , $time
                             , fshow (readRegRes))))
    ) );
  function writeReg (verbosity, addr, data) = rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting writeReg, addr = "
                             , $time
                             , fshow (addr)
                             , ", data = "
                             , fshow (data))))
    , action
        shim.slave.aw.put (AXI4Lite_AWFlit { awaddr: addr
                                           , awprot: 0
                                           , awuser: 0 });
        shim.slave.w.put (AXI4Lite_WFlit { wdata: data
                                         , wstrb: ~0
                                         , wuser: 0 });
      endaction
    , shim.slave.b.drop
    , rWhen ( verbosity > 0
            , rAct ($display ("%0t - Ended writeReg", $time)))
    ) );

  // Debug unit helpers
  //////////////////////////////////////////////////////////////////////////////

  let debugBaseAddr = 32'hf9000000;
  function debugUnitReadReg (verbosity, idx) =
    readReg (verbosity, debugBaseAddr + zeroExtend ({idx, 2'b00}));
  function debugUnitWriteReg (verbosity, idx, data) =
    writeReg (verbosity, debugBaseAddr + zeroExtend ({idx, 2'b00}), data);

  function Recipe debugUnitSendHalt (Integer verbosity) =
    rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting debugUnitSendHalt"
                             , $time )))
    , debugUnitWriteReg (verbosity - 1, 7'h10, 'h80000001)
    , debugUnitReadReg (verbosity - 1, 7'h11)
    , rWhile ( readRegRes[8] == 1'b0
             , debugUnitReadReg (verbosity - 1, 7'h11) )
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended debugUnitSendHalt, value returned = "
                             , $time
                             , fshow (readRegRes))))
    ) );

  function Recipe debugUnitSendResume (Integer verbosity) =
    rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting debugUnitSendResume"
                             , $time )))
    , debugUnitWriteReg (verbosity - 1, 7'h10, 'h40000001)
    , debugUnitReadReg (verbosity - 1, 7'h11)
    , rWhile ( readRegRes[10] == 1'b0
             , debugUnitReadReg (verbosity - 1, 7'h11) )
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended debugUnitSendResume, value returned = "
                             , $time
                             , fshow (readRegRes))))
    ) );

  function Recipe debugUnitSendReset (Integer verbosity, Bool running) =
    rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting debugUnitSendReset, running = "
                             , $time
                             , fshow (running))))
    , debugUnitSendHalt (verbosity - 1)
    , debugUnitWriteReg (verbosity - 1, 7'h10, running ? 'h00000003 : 'h80000003)
    , recipeDelay (5)
    , debugUnitWriteReg (verbosity - 1, 7'h10, running ? 'h00000001 : 'h80000001)
    , recipeDelay (500)
    , debugUnitReadReg (verbosity - 1, 7'h11)
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended debugUnitSendReset, value returned = "
                             , $time
                             , fshow (readRegRes))))
    ) );

  function Recipe debugUnitSendMemRead (Integer verbosity, Bit #(64) addr) =
    rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting debugUnitSendMemRead, addr = "
                             , $time
                             , fshow (addr))))
    , debugUnitWriteReg (verbosity - 1, 7'h17, 'h003207b0)
    , debugUnitWriteReg (verbosity - 1, 7'h38, 'h00150000)
    , debugUnitWriteReg (verbosity - 1, 7'h3a, truncateLSB (addr))
    , debugUnitWriteReg (verbosity - 1, 7'h39, truncate (addr))
    , debugUnitReadReg (verbosity - 1, 7'h3c)
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended debugUnitSendMemRead, value returned = "
                             , $time
                             , fshow (readRegRes))))
    ) );

  function Recipe debugUnitSendMemWrite ( Integer verbosity
                                      , Bit #(64) addr
                                      , Bit #(32) data) =
    rSeq ( rBlock (
      rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Starting debugUnitSendMemWrite, addr = "
                             , $time
                             , fshow (addr)
                             , ", data = "
                             , fshow (data))))
    , debugUnitWriteReg (verbosity - 1, 7'h17, 'h003207b0)
    , debugUnitWriteReg (verbosity - 1, 7'h38, 'h00050000)
    , debugUnitWriteReg (verbosity - 1, 7'h3a, truncateLSB (addr))
    , debugUnitWriteReg (verbosity - 1, 7'h39, truncate (addr))
    , debugUnitWriteReg (verbosity - 1, 7'h3c, data)
    , debugUnitReadReg (verbosity - 1, 7'h38)
    , rWhen ( verbosity > 0
            , rAct ($display ( "%0t - Ended debugUnitSendMemWrite, value returned = "
                             , $time
                             , fshow (readRegRes))))
    ) );

  // fake 16550 helpers
  //////////////////////////////////////////////////////////////////////////////

  let fake16550BaseAddr = 32'hf9030000;
  function fake16550ReadReg (verbosity, idx) =
    readReg (verbosity, fake16550BaseAddr + zeroExtend (idx));
  function fake16550WriteReg (verbosity, idx, data) =
    writeReg (verbosity, fake16550BaseAddr + zeroExtend (idx), data);
  function fake16550ReceiveData (verbosity) =
    readReg (verbosity, fake16550BaseAddr + zeroExtend (4'h0));
  function fake16550TransmitData (verbosity, data) =
    writeReg (verbosity, fake16550BaseAddr + zeroExtend (4'h0), data);

  // Instantiate top recipe
  //////////////////////////////////////////////////////////////////////////////

  Integer verbosity = 2;
  PulseWire done <- mkPulseWire;
  // Set the "AXILiteSimRecipe.bsv" simlink to the "*.AXILiteSimRecipe.bsv" of
  // choice (a "*.AXILiteSimRecipe.bsv" file must define a single `Recipe r`).
  `include "AXILiteSimRecipe.bsv"
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

// Simulation toplevel module
////////////////////////////////////////////////////////////////////////////////

module mkCHERI_BGAS_Top_Sim (Empty);

  // topmodule to simulate
  DE10ProIfc cheri_bgas_top <- mkCHERI_BGAS_Top;

  // H2F_LW driver
  AXI4Lite_Master #( `H2F_LW_ADDR, `H2F_LW_DATA
                   , `H2F_LW_AWUSER, `H2F_LW_WUSER, `H2F_LW_BUSER
                   , `H2F_LW_ARUSER, `H2F_LW_RUSER )
    axiLiteDriver <- mkAXILiteDriver;

  // DDRs
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRB <- mkAXI4Mem (4096, Nothing);
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRC <- mkAXI4Mem (4096, Nothing);
  AXI4_Slave #( `DRAM_ID, `DRAM_ADDR, `DRAM_DATA
              , `DRAM_AWUSER, `DRAM_WUSER, `DRAM_BUSER
              , `DRAM_ARUSER, `DRAM_RUSER )
    fakeDDRD <- mkAXI4Mem (4096, Nothing);

  // connect it all up
  mkConnection (cheri_bgas_top.axls_h2f_lw, axiLiteDriver);
  //mkConnection (cheri_bgas_top.axs_h2f, culDeSac);
  //mkConnection (cheri_bgas_top.axm_f2h, culDeSac);
  mkConnection ( cheri_bgas_top.axm_ddrb
               , debugAXI4_Slave (fakeDDRB, $format ("ddrb")));
  mkConnection ( cheri_bgas_top.axm_ddrc
               , debugAXI4_Slave (fakeDDRC, $format ("ddrc")));
  mkConnection ( cheri_bgas_top.axm_ddrd
               , debugAXI4_Slave (fakeDDRD, $format ("ddrd")));
endmodule

endpackage
