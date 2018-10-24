/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! # Intel 8080 emulator
//! 
//! This crates aims to be a flexible library to embed a 8080 emulator. It is currently used
//! to run a space invaders emulator but could be used for others emulators.
//! 
//! The main struct is [`Proc8080`](proc_state/struct.Proc8080.html) which emulates the state 
//! of a 8080 processor (memory, flags and registers). 
//! 
//! The second useful construct is probably the function 
//! [`read_opcode`](opcode/fn.read_opcode.html) which can be used to build a disassembler backed
//! by this crate [opcode implementation](opcode/enum.OpCode.html). 
//! 
//! Here is an example of such a disassembler : 
//! 
//! ```no_run
//! extern crate intel_8080_emu;
//! 
//! use std::fs::File;
//! use std::io;
//! use intel_8080_emu::opcode::OpCodes;
//! 
//! fn main() {
//!     let file_path: Option<String> = std::env::args().skip(1).next();
//!     match file_path {
//!         None => {
//!             eprintln!("usage: disassembler <file>");
//!             std::process::exit(1);
//!         }
//!         Some(path) => {
//!             let input: File = File::open(path).expect("file not found");
//!             let op_codes = OpCodes::new(io::BufReader::new(input));
//!             let mut count = 0;
//!             for op_code_result in op_codes {
//!                 let op_code = op_code_result.unwrap();
//!                 println!("0x{:04x?} - {}", count, op_code);
//!                 count += op_code.size();
//!             }
//!         }
//!     }
//! }
//! ``` 


pub mod proc_state;
pub mod opcode;
