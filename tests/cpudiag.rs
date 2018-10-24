/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
 
extern crate intel_8080_emu;

use std::fs;
use intel_8080_emu::proc_state::{ Proc8080, InterceptableProc8080, DataBus };
use intel_8080_emu::opcode::{ OpCode, Reg16 };
use std::cell::Cell;

struct DummyBus {}

impl DataBus for DummyBus {

    fn read_port(&self, _port: u8) -> u8 {
        0
    }

    fn write_port(&mut self, _port: u8, _value: u8) {

    }

}

#[test]
fn cpudiag() {

    let program = fs::read("./resources/cpudiag.bin").unwrap();
    let mut memory = Box::new([0;0xffff]);
    memory[0x100..(0x100 + program.len())].copy_from_slice(&program);
    // go to 0x100 at start
    memory[0] = 0xc3;
    memory[1] = 0;
    memory[2] = 0x01;

    let proc8080: Proc8080<DummyBus> = Proc8080::new(memory, DummyBus {});

    let done = Cell::new(false);

    let intercept_opcodes = |cpu: &Proc8080<DummyBus>, op: &OpCode| {
        let flags = cpu.flags();
        println!("{:04x} - {:<12} | A={:<3x?},C={:<6},P={:<6},S={:<6},Z={:<6}", cpu.registers().pc, format!("{}", op), cpu.registers().a, flags.cy, flags.p, flags.s, flags.z);
        match *op {
            OpCode::Call(5) => {
                if cpu.registers().c == 9 {
                    let mut char_addr: u16 = cpu.registers().reg_16_val(Reg16::D) + 3;
                    let message: String = cpu.memory()[(char_addr as usize)..].into_iter()
                        .take_while(|value| **value != '$' as u8).map(|v| char::from(*v)).collect();
                    println!("{:?}", cpu);
                    if message != " CPU IS OPERATIONAL" {
                        panic!(message);
                    } else {
                        done.set(true);
                    }
                }
                true
            }
            _ => false
        }
    };
    
    let mut interceptable_proc = InterceptableProc8080::from_8080(proc8080, intercept_opcodes);

    for _ in 0..1000 {
        interceptable_proc.emulate().unwrap();
        if done.get() {
            break;
        }
    }

    assert!(done.get())
}