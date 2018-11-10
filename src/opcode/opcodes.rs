/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use self::OpCode::*;
use std;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Reg16 {
    B,
    D,
    H,
    SP,
}

impl Display for Reg16 {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OpCode {
    Nop,
    Lxi(Reg16, u16),
    StaxB,
    Inx(Reg16),
    Inr(Register),
    InrM,
    Dcr(Register),
    DcrM,
    Mvi(Register, u8),
    MviM(u8),
    Rlc,

    Dad(Reg16),
    LdaxB,
    Dcx(Reg16),
    Rrc,

    StaxD,
    Ral,

    LdaxD,
    Rar,
    Shld(u16),
    Daa,

    Lhld(u16),
    Cma,
    Sta(u16),
    Stc,

    Lda(u16),
    Cmc,
    Mov(Register, Register),
    MovToM(Register),
    MovFromM(Register),
    Hlt,
    Add(Register),
    AddM,
    Adc(Register),
    AdcM,
    Sub(Register),
    SubM,
    Sbb(Register),
    SbbM,
    Ana(Register),
    AnaM,
    Xra(Register),
    XraM,
    Ora(Register),
    OraM,
    Cmp(Register),
    CmpM,
    Rnz,
    Pop(Reg16),
    Jnz(u16),
    Jmp(u16),
    Cnz(u16),
    Push(Reg16),
    Adi(u8),
    Rst(u8),
    Rz,
    Ret,
    Jz(u16),

    Cz(u16),
    Call(u16),
    Aci(u8),
    Rnc,
    Jnc(u16),
    Out(u8),
    Cnc(u16),
    Sui(u8),
    Rc,

    Jc(u16),
    In(u8),
    Cc(u16),

    Sbi(u8),
    Rpo,
    Jpo(u16),
    Xthl,
    Cpo(u16),
    Ani(u8),
    Rpe,
    Pchl,
    Jpe(u16),
    Xchg,
    Cpe(u16),

    Xri(u8),
    Rp,
    PopPSW,
    Jp(u16),
    Di,
    Cp(u16),
    PushPSW,
    Ori(u8),
    Rm,
    Sphl,
    Jm(u16),
    Ei,
    Cm(u16),

    Cpi(u8),
}

impl OpCode {
    pub fn size(&self) -> u16 {
        match *self {
            Lxi(_, _)
            | Shld(_)
            | Lhld(_)
            | Sta(_)
            | Lda(_)
            | Jnz(_)
            | Jmp(_)
            | Cnz(_)
            | Jz(_)
            | Cz(_)
            | Call(_)
            | Jnc(_)
            | Cnc(_)
            | Jc(_)
            | Cc(_)
            | Jpo(_)
            | Cpo(_)
            | Jpe(_)
            | Cpe(_)
            | Jp(_)
            | Cp(_)
            | Jm(_)
            | Cm(_) => 3,
            Mvi(_, _)
            | Adi(_)
            | Aci(_)
            | Out(_)
            | Sui(_)
            | In(_)
            | Sbi(_)
            | Ani(_)
            | Xri(_)
            | Ori(_)
            | Cpi(_)
            | MviM(_) => 2,
            _ => 1,
        }
    }

    /// Cycles of the opcode as described in the 8080 manual. These are memory cycles and not cpu cycles.
    /// # See also
    /// [states](#method.states)
    pub fn cycles(&self, jumped: bool) -> u64 {
        match *self {
            // Data transfer
            Mov(_, _) => 1,
            MovFromM(_) => 2,
            MovToM(_) => 2,
            Mvi(_, _) => 2,
            MviM(_) => 3,
            Lxi(_, _) => 3,
            Lda(_) => 4,
            Sta(_) => 4,
            Lhld(_) => 5,
            Shld(_) => 5,
            LdaxB => 2,
            LdaxD => 2,
            StaxB => 2,
            StaxD => 2,
            Xchg => 1,

            // Arithmetic
            Add(_) => 1,
            AddM => 2,
            Adi(_) => 2,
            Adc(_) => 1,
            AdcM => 2,
            Aci(_) => 2,
            Sub(_) => 1,
            SubM => 2,
            Sui(_) => 2,
            Sbb(_) => 1,
            SbbM => 2,
            Sbi(_) => 2,
            Inr(_) => 1,
            InrM => 3,
            Dcr(_) => 1,
            DcrM => 3,
            Inx(_) => 1,
            Dcx(_) => 1,
            Dad(_) => 3,
            Daa => 1,

            // Logical
            Ana(_) => 1,
            AnaM => 2,
            Ani(_) => 2,
            Xra(_) => 1,
            XraM => 2,
            Xri(_) => 2,
            Ora(_) => 1,
            OraM => 2,
            Ori(_) => 2,
            Cmp(_) => 1,
            CmpM => 2,
            Cpi(_) => 2,
            Rlc => 1,
            Rrc => 1,
            Ral => 1,
            Rar => 1,
            Cma => 1,
            Cmc => 1,
            Stc => 1,

            // Stack, I/O, and Machine Control
            Push(_) => 3,
            PushPSW => 3,
            Pop(_) => 3,
            PopPSW => 3,
            Xthl => 5,
            Sphl => 1,
            In(_) => 3,
            Out(_) => 3,
            Ei => 1,
            Di => 1,

            // Branch
            Jmp(_) | Jnz(_) | Jz(_) | Jnc(_) | Jc(_) | Jpo(_) | Jpe(_) | Jp(_) | Jm(_) => 3,
            Call(_) => 5,
            Cnz(_) | Cz(_) | Cnc(_) | Cc(_) | Cpo(_) | Cpe(_) | Cp(_) | Cm(_) => if jumped { 5 } else { 3 },
            Ret => 3,
            Rnz | Rz | Rnc | Rc | Rpo | Rpe | Rp | Rm => if jumped { 1 } else { 3 },
            Rst(_) => 3,
            Pchl => 1,

            _ => 1,
        }
    }

    /// States of the opcode as described in the 8080 manual. These are cpu cycles.
    /// # See also
    /// [cycles](#method.cycles)
    pub fn states(&self, jumped: bool) -> u64 {
        match *self {
            // Data transfer
            Mov(_, _) => 5,
            MovFromM(_) => 7,
            MovToM(_) => 7,
            Mvi(_, _) => 7,
            MviM(_) => 10,
            Lxi(_, _) => 10,
            Lda(_) => 13,
            Sta(_) => 13,
            Lhld(_) => 16,
            Shld(_) => 16,
            LdaxB => 7,
            LdaxD => 7,
            StaxB => 7,
            StaxD => 7,
            Xchg => 4,

            // Arithmetic
            Add(_) => 4,
            AddM => 7,
            Adi(_) => 7,
            Adc(_) => 4,
            AdcM => 7,
            Aci(_) => 7,
            Sub(_) => 4,
            SubM => 7,
            Sui(_) => 7,
            Sbb(_) => 4,
            SbbM => 7,
            Sbi(_) => 7,
            Inr(_) => 5,
            InrM => 10,
            Dcr(_) => 5,
            DcrM => 10,
            Inx(_) => 5,
            Dcx(_) => 5,
            Dad(_) => 10,
            Daa => 4,

            // Logical
            Ana(_) => 4,
            AnaM => 7,
            Ani(_) => 7,
            Xra(_) => 4,
            XraM => 7,
            Xri(_) => 7,
            Ora(_) => 7,
            OraM => 7,
            Ori(_) => 7,
            Cmp(_) => 4,
            CmpM => 7,
            Cpi(_) => 7,
            Rlc => 4,
            Rrc => 4,
            Ral => 4,
            Rar => 4,
            Cma => 4,
            Cmc => 4,
            Stc => 4,

            // Stack, I/O, and Machine Control
            Push(_) => 11,
            PushPSW => 11,
            Pop(_) => 10,
            PopPSW => 10,
            Xthl => 18,
            Sphl => 5,
            In(_) => 10,
            Out(_) => 10,
            Ei => 4,
            Di => 4,
            Hlt => 7,
            Nop => 4,

            // Branch
            Jmp(_) | Jnz(_) | Jz(_) | Jnc(_) | Jc(_) | Jpo(_) | Jpe(_) | Jp(_) | Jm(_) => 10,
            Call(_) => 17,
            Cnz(_) | Cz(_) | Cnc(_) | Cc(_) | Cpo(_) | Cpe(_) | Cp(_) | Cm(_) => if jumped { 17 } else { 11 },
            Ret => 10,
            Rnz | Rz | Rnc | Rc | Rpo | Rpe | Rp | Rm => if jumped { 5 } else { 11 },
            Rst(_) => 11,
            Pchl => 5
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            Nop => write!(f, "NOP"),
            Lxi(ref reg, ref v) => write!(f, "LXI {},${:04x}", reg, v),
            StaxB => write!(f, "STAX B"),
            Inx(ref reg) => write!(f, "INX {}", reg),
            Inr(ref reg) => write!(f, "INR {}", reg),
            InrM => write!(f, "INR M"),
            Dcr(ref reg) => write!(f, "DCR {}", reg),
            DcrM => write!(f, "DCR M"),
            Mvi(ref reg, ref v) => write!(f, "MVI {},${:02x?}", reg, v),
            MviM(v) => write!(f, "MVI M,${:02x?}", v),
            Rlc => write!(f, "RLC"),

            Dad(ref reg) => write!(f, "DAD {}", reg),
            LdaxB => write!(f, "LDAX B"),
            Dcx(ref reg) => write!(f, "DCX {}", reg),
            Rrc => write!(f, "RRC"),

            StaxD => write!(f, "STAX D"),
            Ral => write!(f, "RAL"),

            LdaxD => write!(f, "LDAX D"),
            Rar => write!(f, "RAR"),
            Shld(ref v) => write!(f, "SHLD ${:02x?}", v),
            Daa => write!(f, "DAA"),

            Lhld(ref v) => write!(f, "LHLD ${:02x?}", v),
            Cma => write!(f, "CMA"),
            Sta(ref adr) => write!(f, "STA ${:04x?}", adr),
            Stc => write!(f, "STC"),

            Lda(ref adr) => write!(f, "LDA ${:04x?}", adr),
            Cmc => write!(f, "CMC"),
            Mov(ref reg1, ref reg2) => write!(f, "MOV {},{}", reg1, reg2),
            MovToM(reg) => write!(f, "MOV M, {}", reg),
            MovFromM(reg) => write!(f, "MOV {},M", reg),
            Hlt => write!(f, "HLT"),
            Add(ref reg) => write!(f, "ADD {}", reg),
            AddM => write!(f, "ADD M"),
            Adc(ref reg) => write!(f, "ADC {}", reg),
            AdcM => write!(f, "ADC M"),
            Sub(ref reg) => write!(f, "SUB {}", reg),
            SubM => write!(f, "SUB M"),
            Sbb(ref reg) => write!(f, "SBB {}", reg),
            SbbM => write!(f, "SBB M"),
            Ana(ref reg) => write!(f, "ANA {}", reg),
            AnaM => write!(f, "ANA M"),
            Xra(ref reg) => write!(f, "XRA {}", reg),
            XraM => write!(f, "XRA M"),
            Ora(ref reg) => write!(f, "ORA {}", reg),
            OraM => write!(f, "ORA M"),
            Cmp(ref reg) => write!(f, "CMP {}", reg),
            CmpM => write!(f, "CMP M"),
            Rnz => write!(f, "RNZ"),
            Pop(ref reg) => write!(f, "POP {}", reg),
            Jnz(ref adr) => write!(f, "JNZ ${:04x?}", adr),
            Jmp(ref adr) => write!(f, "JMP ${:04x?}", adr),
            Cnz(ref adr) => write!(f, "CNZ ${:04x?}", adr),
            Push(ref reg) => write!(f, "PUSH {}", reg),
            Adi(ref v) => write!(f, "ADI ${:02x?}", v),
            Rst(ref v) => write!(f, "RST {}", v),
            Rz => write!(f, "RZ"),
            Ret => write!(f, "RET"),
            Jz(ref adr) => write!(f, "JZ ${:04x?}", adr),

            Cz(ref adr) => write!(f, "CZ ${:04x?}", adr),
            Call(ref adr) => write!(f, "CALL ${:04x?}", adr),
            Aci(ref v) => write!(f, "ACI ${:02x?}", v),
            Rnc => write!(f, "RNC"),
            Jnc(ref adr) => write!(f, "JNC ${:04x?}", adr),
            Out(ref v) => write!(f, "OUT ${:02x?}", v),
            Cnc(ref adr) => write!(f, "CNC ${:04x?}", adr),
            Sui(ref v) => write!(f, "SUI {:02x?}", v),
            Rc => write!(f, "RC"),

            Jc(ref adr) => write!(f, "JC ${:04x?}", adr),
            In(ref v) => write!(f, "IN ${:02x?}", v),
            Cc(ref adr) => write!(f, "CC ${:04x?}", adr),

            Sbi(ref v) => write!(f, "SBI ${:02x?}", v),
            Rpo => write!(f, "RPO"),
            Jpo(ref adr) => write!(f, "JPO ${:04x?}", adr),
            Xthl => write!(f, "XTHL"),
            Cpo(ref adr) => write!(f, "CPO ${:04x?}", adr),
            Ani(ref v) => write!(f, "ANI ${:02x?}", v),
            Rpe => write!(f, "RPE"),
            Pchl => write!(f, "PCHL"),
            Jpe(ref adr) => write!(f, "JPE ${:04x?}", adr),
            Xchg => write!(f, "XCHG"),
            Cpe(ref adr) => write!(f, "CPE ${:04x?}", adr),

            Xri(ref v) => write!(f, "XRI ${:02x?}", v),
            Rp => write!(f, "RP"),
            PopPSW => write!(f, "POP PSW"),
            Jp(ref adr) => write!(f, "JP ${:04x?}", adr),
            Di => write!(f, "DI"),
            Cp(ref adr) => write!(f, "CP ${:04x?}", adr),
            PushPSW => write!(f, "PUSH PSW"),
            Ori(ref v) => write!(f, "ORI ${:02x?}", v),
            Rm => write!(f, "RM"),
            Sphl => write!(f, "SPHL"),
            Jm(ref adr) => write!(f, "JM ${:04x?}", adr),
            Ei => write!(f, "EI"),
            Cm(ref adr) => write!(f, "CM ${:04x?}", adr),

            Cpi(ref v) => write!(f, "CPI ${:02x?}", v),
        }
    }
}
