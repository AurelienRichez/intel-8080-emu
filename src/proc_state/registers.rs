/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use opcode::{Register, Reg16};

#[derive(Debug)]
pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
}

impl Registers {

    pub fn apply_mov(&mut self, reg1: Register, reg2: Register) {
        let reg_value = self.reg_val(reg2);
        self.set_reg_val(reg1, reg_value);
    }

    pub fn reg_val(&self, reg: Register) -> u8 {
        match reg {
            Register::A => self.a,
            Register::B => self.b,
            Register::C => self.c,
            Register::D => self.d,
            Register::E => self.e,
            Register::H => self.h,
            Register::L => self.l,
        }
    }

    pub fn set_reg_val(&mut self, reg: Register, value: u8) {
        match reg {
            Register::A => self.a = value,
            Register::B => self.b = value,
            Register::C => self.c = value,
            Register::D => self.d = value,
            Register::E => self.e = value,
            Register::H => self.h = value,
            Register::L => self.l = value,
        }
    }

    pub fn reg_16_val(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::B =>  ((self.b as u16) << 8) + self.c as u16,
            Reg16::D => ((self.d as u16) << 8) + self.e as u16,
            Reg16::H => ((self.h as u16) << 8) + self.l as u16,
            Reg16::SP => self.sp,
        }
    }
    
    pub fn set_reg_16_val(&mut self, reg: Reg16, value: u16) {
        match reg {
            Reg16::B =>  {
                self.b = (value >> 8) as u8;
                self.c = value as u8;
            }
            Reg16::D => {
                self.d = (value >> 8) as u8;
                self.e = value as u8;
            }
            Reg16::H => {
                self. h = (value >> 8) as u8;
                self.l = value as u8;
            },
            Reg16::SP => self.sp = value,
        }
    }
}

impl Default for Registers {
    
    fn default() -> Registers {
        Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            sp: 0xffff,
            pc: 0,
        }
    }
}