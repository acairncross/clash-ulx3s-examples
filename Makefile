BUILDDIR ?= build

TOPMODS := stackMachine counter ram dvi
SYNTHCFGS := $(addprefix $(BUILDDIR)/,$(TOPMODS:=.json))
ROUTECFGS := $(addprefix $(BUILDDIR)/,$(TOPMODS:=_out.config))
BITSTREAMS := $(addprefix $(BUILDDIR)/,$(TOPMODS:=.bit))

.PHONY: all
all: $(BITSTREAMS)

.PHONY: clean
clean:
	rm -rf verilog $(BUILDDIR)

$(TOPMODS): %: $(BUILDDIR)/%.bit
	@echo Built bitstream $<

$(BUILDDIR)/%.bit: $(BUILDDIR)/%_out.config
	ecppack $< $@

$(BUILDDIR)/%_out.config: $(BUILDDIR)/%.json
	nextpnr-ecp5 --85k --json $< \
		--lpf ulx3s_v20.lpf \
		--textcfg $@ \
		--package CABGA381 

$(BUILDDIR)/%.json: verilog | $(BUILDDIR)
	yosys -p "synth_ecp5 -json $@" verilog/Top.$*/$*.v

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

.PHONY: verilog
verilog: clash-ulx3s-examples.cabal src/RAM.hs src/StackMachine.hs src/Top.hs src/UART.hs src/Utils.hs src/DVI.hs
	cabal run --write-ghc-environment-files=always clash -- \
	  --verilog Top -fclash-clear -fclash-no-cache

.PHONY: update
update:
	cabal run -v0 scripts/Img2Mem.hs -- res/tile.png > res/tile.mem
