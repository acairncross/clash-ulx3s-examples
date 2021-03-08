# clash-ulx3s-examples

This is a collection of Clash designs, build scripts and Haskell programs that
you can use to synthesise a bitstream, program it on to an FPGA and interact
with it.

Much of the designs are FPGA/board agnostic, but there are a few ECP5 specific
primitives, and the top entities/synthesis annotations and Makefile are
specific to the ULX3S board.

## Example designs

The bitstream for each example can be built with `make <example name>`. Note
that you will need Yosys and nextpnr built with ECP5 support.

### `counter`

A counter displayed on the on board LEDs.

### `ram`

A block RAM that you can interact with (read and write) over a USB connection
using a REPL. Run the REPL with `cabal run ram-repl`.

### `stackMachine`

A simple stack machine processor that you can interact with (evaluate arithmetic
expressions) over a USB connection using a REPL. Run the REPL with `cabal run
stack-machine-repl`.

### `dvi`

Uses a DVI transmitter to display test pattern on a monitor connected via the
GPDI port.
