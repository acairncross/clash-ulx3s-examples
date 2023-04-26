# clash-ulx3s-examples

This is a collection of Clash designs, build scripts and Haskell programs that
you can use to synthesise a bitstream, program it on to an FPGA and interact
with it.

Much of the designs are FPGA/board agnostic, but there are a few ECP5 specific
primitives, and the top entities/synthesis annotations and Makefile are
specific to the ULX3S board.

## Dependencies

You will need the following system dependencies:

- GHC with a suitable version of the `base` library. You can use
  [GHCup](https://www.haskell.org/ghcup/) to install specific GHC versions.
  The project has been tested with GHC 8.10.
- Yosys tools. These can be installed all together with the
  [OSS CAD suite](https://github.com/YosysHQ/oss-cad-suite-build).
  - [Yosys](https://github.com/YosysHQ/yosys) for `yosys`.
  - [Nextpnr](https://github.com/YosysHQ/nextpnr) with ECP5 support for
    `nextpnr-ecp5`.
  - [Project Trellis](https://github.com/YosysHQ/prjtrellis) for `ecppack`.
- SDL, e.g. `libsdl2-dev` in Ubuntu, for simulating `snake`.

Depending on your version of SDL, you might need to configure the project to
use a specific version of the Haskell `sdl2` library, e.g. the version of
`libsdl2-dev` is too old for the most recent Haskell `sdl2` library, so run:

```
$ cabal configure --constraint="sdl2 <2.5.5.0 && >=2.5.0.0"
```

before any of the build commands.

## Example designs

The bitstream for each example can be built with `make <example name>`.

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

### `snake`

A game of Snake using the directional buttons as input and GPDI output for the
display. Run an SDL based simulation with `cabal run snake-sim`.
