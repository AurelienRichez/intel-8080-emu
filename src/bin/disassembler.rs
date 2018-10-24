/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
 
extern crate intel_8080_emu;

use std::fs::File;
use std::io;
use intel_8080_emu::opcode;

fn main() {
    let file_path: Option<String> = std::env::args().skip(1).next();
    match file_path {
        None => {
            eprintln!("usage: disassembler <file>");
            std::process::exit(1);
        }
        Some(path) => {
            let input: File = File::open(path).expect("file not found");
            let op_codes = opcode::OpCodes::new(io::BufReader::new(input));
            let mut count = 0;
            for op_code_result in op_codes {
                let op_code = op_code_result.unwrap();
                println!("0x{:04x?} - {}", count, op_code);
                count += op_code.size();
            }
        }
    }
}
