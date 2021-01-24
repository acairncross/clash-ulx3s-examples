BUILDDIR ?= build

TOPMODS := topEntity topCounter topRam
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
	yosys -p "synth_ecp5 -json $@" verilog/Top/$*/$*.v

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

verilog: clash-ulx3s-examples.cabal src/RAM.hs src/StackMachine.hs src/Top.hs src/UART.hs src/Utils.hs
	cabal run --write-ghc-environment-files=always clash -- --verilog Top

doc/clash.pdf: doc/clash.tex
	pdflatex -output-directory doc doc/clash.tex
