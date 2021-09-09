#-
# Copyright (c) 2021 Alexandre Joannou
# All rights reserved.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

BSVSRCDIR = bluespec
VIPBUNDLEDIR = $(CURDIR)/vipbundle
VIPBUNDLE = $(VIPBUNDLEDIR)/vipbundle
QPF = $(CURDIR)/DE10Pro-cheri-bgas.qpf
export VDIR = $(CURDIR)/cheri-bgas-rtl

all: synthesize

synthesize: gen-ip
	time quartus_sh --flow compile $(QPF)

gen-ip: gen-bluespec-quartus-ip
	quartus_ipgenerate $(QPF)

gen-bluespec-quartus-ip: $(VIPBUNDLE) gen-bluespec-rtl
	$(VIPBUNDLEDIR)/vipbundle \
      -f quartus_ip_tcl \
      -o $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl \
	  $(VDIR)/mkCHERI_BGAS_Top_Sig.v

$(VIPBUNDLE):
	$(MAKE) -C $(VIPBUNDLEDIR) vipbundle

gen-bluespec-rtl:
	$(MAKE) -C $(BSVSRCDIR) rtl

.PHONY: clean mrproper

clean-bluespec-rtl:
	$(MAKE) -C $(BSVSRCDIR) clean

clean-vipbundle:
	$(MAKE) -C $(VIPBUNDLEDIR) clean

clean-bluespec-quartus-ip:
	rm -f $(CURDIR)/mkCHERI_BGAS_Top_Sig_hw.tcl

clean-ip-gen:
	rm -rf $(CURDIR)/ip/reset_release/
	rm -rf $(CURDIR)/ip/toplevel/CHERI_BGAS_Top/
	rm -rf $(CURDIR)/ip/toplevel/clock_in/
	rm -rf $(CURDIR)/ip/toplevel/ddrb_mm_clock_crossing_bridge/
	rm -rf $(CURDIR)/ip/toplevel/emif_hps_ddra/
	rm -rf $(CURDIR)/ip/toplevel/emif_ddrb/
	rm -rf $(CURDIR)/ip/toplevel/hps/
	rm -rf $(CURDIR)/ip/toplevel/reset_in/
	rm -rf $(CURDIR)/toplevel/

clean: clean-bluespec-rtl clean-bluespec-quartus-ip clean-vipbundle clean-ip-gen
	rm -rf $(CURDIR)/qdb $(CURDIR)/synth_dumps $(CURDIR)/tmp-clearbox

mrproper-vipbundle:
	$(MAKE) -C $(VIPBUNDLEDIR) mrproper

mrproper-bluespec-rtl:
	$(MAKE) -C $(BSVSRCDIR) mrproper

mrproper: clean mrproper-bluespec-rtl #mrproper-vipbundle
	rm -rf $(CURDIR)/output_files
