all: $(addprefix $(CURDIR)/tools/, cheri-bgas-fuse-devfs fmem-uart forever-splice jtagvpi_to_fmemdmi openocd fmem)

mrproper: mrproper-cheri-bgas-fuse-devfs mrproper-forever-splice mrproper-jtagvpi_to_fmemdmi mrproper-openocd

####

RUN ?= $(shell basename $(shell mktemp -udp $(CURDIR)/results run.XXXXXX))

MIBENCH_DIR := /local/scratch/ldh35/mibench2
#MIBENCH_NAMES := $(notdir $(wildcard $(MIBENCH_DIR)/*))
# These ones actually work and run for a long time
MIBENCH_NAMES := adpcm_decode adpcm_encode basicmath blowfish dijkstra fft picojpeg qsort

gen-mibench-hex: $(MIBENCH_NAMES:%=gen-mibench-hex-%) ;

gen-mibench-hex-%:
	elfmanip.py -o $(MIBENCH_DIR)/$*/mem.hex -s 0xc0000000 -i 0x40000000 $(MIBENCH_DIR)/$*/$*.elf to-hex

run-mibench: $(MIBENCH_NAMES:%=run-mibench-%) ;

run-mibench-%:
	./run-sim.sh $(RUN) $* "$(MIBENCH_DIR)/$*/mem.hex" $(CURDIR)/results

####

PREFETCHER_NAMES := stride stride_adaptive all_in_cap cheri_stride cap_bitmap cap_ptr

compile-prefetcher: 
	$(MAKE) $(PREFETCHER_NAMES:%=compile-prefetcher-%)

compile-prefetcher-%: $(CURDIR)/bin/%/sim_CHERI_BGAS ;

.PRECIOUS: $(CURDIR)/bin/%/sim_CHERI_BGAS
$(CURDIR)/bin/%/sim_CHERI_BGAS:
	touch $(CURDIR)/../Toooba/src_Core/RISCY_OOO/coherence/src/prefetcher/Prefetcher_top.bsv
	DATA_PREFETCHER_TYPE=$(shell echo $* | tr '[:lower:]' '[:upper:]') $(MAKE) -j12 -C ../ bluesim
	mkdir -p $(CURDIR)/bin/$*
	cp $(CURDIR)/../build/simdir/sim_CHERI_BGAS* $(CURDIR)/bin/$*/

run-prefetcher-mibench: $(foreach pref, $(PREFETCHER_NAMES), $(addprefix run-prefetcher-mibench-${pref}-, $(MIBENCH_NAMES))) ;

run-prefetcher-mibench-%:
	PREFETCHER=$(word 1,$(subst -, ,$*)) BENCHMARK=$(word 2,$(subst -, ,$*)) ;\
	./run-sim.sh $(RUN)-$$PREFETCHER $$BENCHMARK $(MIBENCH_DIR)/$$BENCHMARK/mem.hex $(CURDIR)/results $(CURDIR)/bin/$$PREFETCHER/sim_CHERI_BGAS

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
