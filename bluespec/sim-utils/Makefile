all: $(addprefix $(CURDIR)/tools/, cheri-bgas-fuse-devfs fmem-uart forever-splice jtagvpi_to_fmemdmi openocd fmem)

mrproper: mrproper-cheri-bgas-fuse-devfs mrproper-forever-splice mrproper-jtagvpi_to_fmemdmi mrproper-openocd

####

BIN ?= default
RUN ?= $(shell basename $(shell mktemp -udp $(CURDIR)/results run.XXXXXX))
TOUT ?= 

MIBENCH_DIR := /local/scratch/ldh35/mibench2-workspace
OLDEN_DIR := /local/scratch/ldh35/olden-baremetal-cheri-riscv
#MIBENCH_NAMES := $(notdir $(wildcard $(MIBENCH_DIR)/*))
# These ones actually work
#MIBENCH_NAMES := adpcm_decode adpcm_encode aes basicmath blowfish dijkstra fft patricia picojpeg qsort rc4
#MIBENCH_NAMES := dijkstra_capchain patricia
MIBENCH_NAMES := patricia
OLDEN_NAMES := tsp treeadd treeadd_frag power perimeter health em3d bisort bh

genhex: $(MIBENCH_NAMES:%=genhex.mibench.%) $(OLDEN_NAMES:%=genhex.olden.%) ;

genhex.mibench.%:
	elfmanip.py -o $(MIBENCH_DIR)/$*/mem.hex -s 0xc0000000 -i 0x40000000 $(MIBENCH_DIR)/$*/main.elf to-hex

genhex.olden.%:
	elfmanip.py -o $(OLDEN_DIR)/$*/mem.hex -s 0xc0000000 -i 0x40000000 $(OLDEN_DIR)/$*/main.elf to-hex

####

PREFETCHER_CONFIGS := cap_spatial_ptr-l1 none-none cap_chaser-l1ll cap_chaser_split-l1ll cap_ptr-l1 stride_adaptive-l1 cap_chaser_split_stride-l1ll
#PREFETCHER_CONFIGS := \
#	cap_chaser_allin_h_always_m_always-l1ll \
#	cap_chaser_allin_h_50_m_50-l1ll \
#	cap_chaser_allin_h_50_m_always-l1ll \
#	cap_chaser_allin_h_75_m_75-l1ll \
#	cap_chaser_allin_h_75_m_50-l1ll \
#	cap_chaser_allin_h_75_m_always-l1ll \
#	cap_chaser_allin_h_never_m_never-l1ll \
#	cap_chaser_allin_h_never_m_75-l1ll \
#	cap_chaser_allin_h_never_m_50-l1ll \
#	cap_chaser_allin_h_never_m_always-l1ll

CACHE_SIZE ?= LOUIS

compile: $(foreach pref_type, $(PREFETCHER_CONFIGS), compile.prefetcher-location.${pref_type}) ;
	
compile.prefetcher-location.%: $(CURDIR)/bin/$(BIN)/%/sim_CHERI_BGAS ;

.PRECIOUS: $(CURDIR)/bin/$(BIN)/%/sim_CHERI_BGAS
$(CURDIR)/bin/$(BIN)/%/sim_CHERI_BGAS:
	-touch $(CURDIR)/../Toooba/src_Core/RISCY_OOO/coherence/src/prefetcher/*
	CACHE_SIZE=$(CACHE_SIZE) \
	DATA_PREFETCHER_TYPE=$(shell echo $(word 1,$(subst -, ,$*)) | tr '[:lower:]' '[:upper:]') \
	DATA_PREFETCHER_LOCATION=$(shell echo $(word 2,$(subst -, ,$*)) | tr '[:lower:]' '[:upper:]') \
		$(MAKE) -j12 -C ../ bluesim
	mkdir -p $(CURDIR)/bin/$(BIN)/$*
	cp $(CURDIR)/../build/simdir/sim_CHERI_BGAS* $(CURDIR)/bin/$(BIN)/$*/

run: $(foreach pref_type, $(PREFETCHER_CONFIGS), run.prefetcher-location.${pref_type}) ;

run.prefetcher-location.%: $(addprefix run.prefetcher-location-mibench.%-, $(MIBENCH_NAMES)) $(addprefix run.prefetcher-location-olden.%-, $(OLDEN_NAMES)) ;

run.prefetcher-location-mibench.%:
	PREFETCHER_TYPE=$(word 1,$(subst -, ,$*)) PREFETCHER_LOCATION=$(word 2,$(subst -, ,$*)) BENCHMARK=$(word 3,$(subst -, ,$*)) ;\
	./run-sim.sh $(RUN)-$$PREFETCHER_TYPE-$$PREFETCHER_LOCATION $$BENCHMARK $(MIBENCH_DIR)/$$BENCHMARK/mem.hex $(CURDIR)/results $(CURDIR)/bin/$(BIN)/$$PREFETCHER_TYPE-$$PREFETCHER_LOCATION/sim_CHERI_BGAS $(TOUT)

run.prefetcher-location-olden.%:
	PREFETCHER_TYPE=$(word 1,$(subst -, ,$*)) PREFETCHER_LOCATION=$(word 2,$(subst -, ,$*)) BENCHMARK=$(word 3,$(subst -, ,$*)) ;\
	./run-sim.sh $(RUN)-$$PREFETCHER_TYPE-$$PREFETCHER_LOCATION $$BENCHMARK $(OLDEN_DIR)/$$BENCHMARK/mem.hex $(CURDIR)/results $(CURDIR)/bin/$(BIN)/$$PREFETCHER_TYPE-$$PREFETCHER_LOCATION/sim_CHERI_BGAS $(TOUT)

####

gen-cheri-bgas-fuse-devfs: $(CURDIR)/tools/cheri-bgas-fuse-devfs

$(CURDIR)/tools/cheri-bgas-fuse-devfs: $(CURDIR)/cheri-bgas-fuse-devfs/cheri-bgas-fuse-devfs
	mkdir -p $(CURDIR)/tools
	cp $< $@

$(CURDIR)/cheri-bgas-fuse-devfs/cheri-bgas-fuse-devfs:
	$(MAKE) -C $(CURDIR)/cheri-bgas-fuse-devfs

mrproper-cheri-bgas-fuse-devfs:
	rm -rf $(CURDIR)/tools/cheri-bgas-fuse-devfs
	$(MAKE) -C $(CURDIR)/cheri-bgas-fuse-devfs clean

####

gen-fmem-uart: $(CURDIR)/tools/fmem-uart

$(CURDIR)/tools/fmem-uart: $(CURDIR)/fmem-uart/obj/fmem-uart
	mkdir -p $(CURDIR)/tools
	cp $< $@

$(CURDIR)/fmem-uart/obj/fmem-uart:
	$(MAKE) -C $(CURDIR)/fmem-uart

mrproper-fmem-uart:
	rm -rf $(CURDIR)/tools/fmem-uart
	$(MAKE) -C $(CURDIR)/fmem-uart clean

####

gen-forever-splice: $(CURDIR)/tools/forever-splice
$(CURDIR)/tools/forever-splice: $(CURDIR)/forever-splice/forever-splice
	mkdir -p $(CURDIR)/tools
	cp $< $@

$(CURDIR)/forever-splice/forever-splice:
	$(MAKE) -C $(CURDIR)/forever-splice

mrproper-forever-splice:
	rm -rf $(CURDIR)/tools/forever-splice
	$(MAKE) -C $(CURDIR)/forever-splice clean

####

gen-jtagvpi_to_fmemdmi: $(CURDIR)/tools/jtagvpi_to_fmemdmi
$(CURDIR)/tools/jtagvpi_to_fmemdmi: $(CURDIR)/jtagvpi_to_fmemdmi/jtagvpi_to_fmemdmi
	mkdir -p $(CURDIR)/tools
	cp $< $@

$(CURDIR)/jtagvpi_to_fmemdmi/jtagvpi_to_fmemdmi:
	$(MAKE) -C $(CURDIR)/jtagvpi_to_fmemdmi

mrproper-jtagvpi_to_fmemdmi:
	rm -rf $(CURDIR)/tools/jtagvpi_to_fmemdmi
	$(MAKE) -C $(CURDIR)/jtagvpi_to_fmemdmi mrproper

####

gen-openocd: $(CURDIR)/tools/openocd
$(CURDIR)/tools/openocd: /tmp/openocd/src/openocd
	mkdir -p $(CURDIR)/tools
	cp $< $@

/tmp/openocd/src/openocd:
	rm -rf /tmp/openocd
	git clone --depth=1 --branch latest https://github.com/openocd-org/openocd.git /tmp/openocd
	# last tested with commit 8a37230
	cd /tmp/openocd && git submodule update --init --recursive && \
	./bootstrap && ./configure --enable-jtag_vpi
	$(MAKE) -C /tmp/openocd

mrproper-openocd:
	rm -rf $(CURDIR)/tools/openocd
	$(MAKE) -C /tmp/openocd clean

####

gen-fmem: $(CURDIR)/tools/fmem
$(CURDIR)/tools/fmem: /tmp/fmem/fmem
	mkdir -p $(CURDIR)/tools
	cp $< $@

/tmp/fmem/fmem:
	rm -rf /tmp/fmem
	git clone --depth=1 --branch main https://github.com/CTSRD-CHERI/fmem.git /tmp/fmem
	cd /tmp/fmem && git submodule update --init --recursive && \
	$(MAKE) -C /tmp/fmem

mrproper-fmem:
	rm -rf $(CURDIR)/tools/fmem
	$(MAKE) -C /tmp/fmem clean
