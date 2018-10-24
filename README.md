Intel 8080 emu
--------
[![Crates.io](https://img.shields.io/crates/v/intel-8080-emu.svg?style=flat-square)](https://crates.io/crates/intel-8080-emu)

An intel 8080 emulator library (another one). Initially implemeted to build a space invaders 
emulator (another another one).

This library provides provides utilities to parse 8080 binary and simulate an 
[8080 microprocessor](https://en.wikipedia.org/wiki/Intel_8080). All op codes are implemented and 
there is no external dependencies.


Quickstart
----------

The main struct is `intel_8080_emu::proc_state::Proc8080`. It needs basically two things to work :
 - The memory, a simple `Box<[u8]>` containing the rom and the ram.
 - A structure implementing `intel_8080_emu::proc_state::DataBus` which handles the `IN` and `OUT`
calls.

```rust
use intel_8080_emu::proc_state::Proc8080;
use foo::bar::MyCustomDataBus;
use std;

// load your rom as an array
let rom: [u8] = load_rom();

// copy the rom into the 8008 memory
let mut memory = Box::new([0x00; 0xffff]); 
memory[0..rom.len()].copy_from_slice(&rom);

let i8080 = Proc8080::new(memory, data_bus);

// we're ready ! 
// Here is naive way to slow down the simulation so that it matches the original speed of the 8080
let mut cycles = i8080.cycles();
loop {
    // emulates runs one "step" of the simulation by running the next opcode, mutating the 
    // processor state accordingly and increasing the cycle count
    i8080.emulate();
    // you can manage time with Proc8080:cycles()
    // A cycle for the 8080 took approximately 500 nanoseconds
    std::thread::sleep(std::time::Duration::from_nanos(500) * (i8080.cycles() - cycles)
}   

```

Possible improvements
----
 - Maybe better memory handling. It is currently a simple `Box<[u8]>` and does not distinguish 
 between ROM and RAM.