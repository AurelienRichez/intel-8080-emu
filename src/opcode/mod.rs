/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

mod opcodes;
pub use self::opcodes::OpCode;
pub use self::opcodes::Register;
pub use self::opcodes::Reg16;

use std::io::BufRead;
use std::fmt;
use std::error::Error;
use std;

use self::opcodes::OpCode::*;
use self::opcodes::Register::*;

/// A parser which takes a slice of 3 bytes and cannot fail.
pub fn read_opcode_safe(bytes: &[u8;3]) -> OpCode {
    read_opcode(bytes).unwrap()
}

/// Read a slice of bytes and returns an opcode, possibly with its data.
/// 
/// Unknown opcodes are interpreted as NOP. The parsing might fail if the slice is too short.
/// 
pub fn read_opcode(bytes: &[u8]) -> Result<OpCode, UnexpectedEndOfInput> {
    bytes.get(0).map(|b| match b {
        0x01 => expect_data_u16(bytes).map(|v| Lxi(Reg16::B, v)),
        0x02 => Ok(StaxB),
        0x03 => Ok(Inx(Reg16::B)),
        0x04 => Ok(Inr(B)),
        0x05 => Ok(Dcr(B)),
        0x06 => expect_data_byte(bytes).map(|v| Mvi(B, v)),
        0x07 => Ok(Rlc),

        0x09 => Ok(Dad(Reg16::B)),
        0x0a => Ok(LdaxB),
        0x0b => Ok(Dcx(Reg16::B)),
        0x0c => Ok(Inr(C)),
        0x0d => Ok(Dcr(C)), 
        0x0e => expect_data_byte(bytes).map(|v| Mvi(C, v)),
        0x0f => Ok(Rrc),

        0x11 => expect_data_u16(bytes).map(|v| Lxi(Reg16::D, v)),
        0x12 => Ok(StaxD),
        0x13 => Ok(Inx(Reg16::D)),
        0x14 => Ok(Inr(D)),
        0x15 => Ok(Dcr(D)),
        0x16 => expect_data_byte(bytes).map(|v| Mvi(D, v)),
        0x17 => Ok(Ral),

        0x19 => Ok(Dad(Reg16::D)),
        0x1a => Ok(LdaxD),
        0x1b => Ok(Dcx(Reg16::D)),
        0x1c => Ok(Inr(E)),
        0x1d => Ok(Dcr(E)),
        0x1e => expect_data_byte(bytes).map(|v| Mvi(E, v)),
        0x1f => Ok(Rar),

        0x21 => expect_data_u16(bytes).map(|v| Lxi(Reg16::H, v)),
        0x22 => expect_data_u16(bytes).map(Shld),
        0x23 => Ok(Inx(Reg16::H)),
        0x24 => Ok(Inr(H)),
        0x25 => Ok(Dcr(H)),
        0x26 => expect_data_byte(bytes).map(|v| Mvi(H, v)),
        0x27 => Ok(Daa),

        0x29 => Ok(Dad(Reg16::H)),
        0x2a => expect_data_u16(bytes).map(Lhld),
        0x2b => Ok(Dcx(Reg16::H)),
        0x2c => Ok(Inr(L)),
        0x2d => Ok(Dcr(L)),
        0x2e => expect_data_byte(bytes).map(|v| Mvi(L, v)),
        0x2f => Ok(Cma),
        
        0x31 => expect_data_u16(bytes).map(|v| Lxi(Reg16::SP, v)),
        0x32 => expect_data_u16(bytes).map(Sta),
        0x33 => Ok(Inx(Reg16::SP)),
        0x34 => Ok(InrM),
        0x35 => Ok(DcrM),
        0x36 => expect_data_byte(bytes).map(|v| MviM(v)),
        0x37 => Ok(Stc),

        0x39 => Ok(Dad(Reg16::SP)),
        0x3a => expect_data_u16(bytes).map(Lda),
        0x3b => Ok(Dcx(Reg16::SP)),
        0x3c => Ok(Inr(A)),
        0x3d => Ok(Dcr(A)),
        0x3e => expect_data_byte(bytes).map(|v| Mvi(A, v)),
        0x3f => Ok(Cmc),
        0x40 => Ok(Mov(B, B)),
        0x41 => Ok(Mov(B, C)),
        0x42 => Ok(Mov(B, D)),
        0x43 => Ok(Mov(B, E)),
        0x44 => Ok(Mov(B, H)),
        0x45 => Ok(Mov(B, L)),
        0x46 => Ok(MovFromM(B)),
        0x47 => Ok(Mov(B, A)),
        0x48 => Ok(Mov(C, B)),
        0x49 => Ok(Mov(C, C)),
        0x4a => Ok(Mov(C, D)),
        0x4b => Ok(Mov(C, E)),
        0x4c => Ok(Mov(C, H)),
        0x4d => Ok(Mov(C, L)),
        0x4e => Ok(MovFromM(C)),
        0x4f => Ok(Mov(C, A)),
        0x50 => Ok(Mov(D, B)),
        0x51 => Ok(Mov(D, C)),
        0x52 => Ok(Mov(D, D)),
        0x53 => Ok(Mov(D, E)),
        0x54 => Ok(Mov(D, H)),
        0x55 => Ok(Mov(D, L)),
        0x56 => Ok(MovFromM(D)),
        0x57 => Ok(Mov(D, A)),
        0x58 => Ok(Mov(E, B)),
        0x59 => Ok(Mov(E, C)),
        0x5a => Ok(Mov(E, D)),
        0x5b => Ok(Mov(E, E)),
        0x5c => Ok(Mov(E, H)),
        0x5d => Ok(Mov(E, L)),
        0x5e => Ok(MovFromM(E)),
        0x5f => Ok(Mov(E, A)),
        0x60 => Ok(Mov(H, B)),
        0x61 => Ok(Mov(H, C)),
        0x62 => Ok(Mov(H, D)),
        0x63 => Ok(Mov(H, E)),
        0x64 => Ok(Mov(H, H)),
        0x65 => Ok(Mov(H, L)),
        0x66 => Ok(MovFromM(H)),
        0x67 => Ok(Mov(H, A)),
        0x68 => Ok(Mov(L, B)),
        0x69 => Ok(Mov(L, C)),
        0x6a => Ok(Mov(L, D)),
        0x6b => Ok(Mov(L, E)),
        0x6c => Ok(Mov(L, H)),
        0x6d => Ok(Mov(L, L)),
        0x6e => Ok(MovFromM(L)),
        0x6f => Ok(Mov(L, A)),
        0x70 => Ok(MovToM(B)),
        0x71 => Ok(MovToM(C)),
        0x72 => Ok(MovToM(D)),
        0x73 => Ok(MovToM(E)),
        0x74 => Ok(MovToM(H)),
        0x75 => Ok(MovToM(L)),
        0x76 => Ok(Hlt),
        0x77 => Ok(MovToM(A)),
        0x78 => Ok(Mov(A, B)),
        0x79 => Ok(Mov(A, C)),
        0x7a => Ok(Mov(A, D)),
        0x7b => Ok(Mov(A, E)),
        0x7c => Ok(Mov(A, H)),
        0x7d => Ok(Mov(A, L)),
        0x7e => Ok(MovFromM(A)),
        0x7f => Ok(Mov(A, A)),
        0x80 => Ok(Add(B)),
        0x81 => Ok(Add(C)),
        0x82 => Ok(Add(D)),
        0x83 => Ok(Add(E)),
        0x84 => Ok(Add(H)),
        0x85 => Ok(Add(L)),
        0x86 => Ok(AddM),
        0x87 => Ok(Add(A)),
        0x88 => Ok(Adc(B)),
        0x89 => Ok(Adc(C)),
        0x8a => Ok(Adc(D)),
        0x8b => Ok(Adc(E)),
        0x8c => Ok(Adc(H)),
        0x8d => Ok(Adc(L)),
        0x8e => Ok(AdcM),
        0x8f => Ok(Adc(A)),
        0x90 => Ok(Sub(B)),
        0x91 => Ok(Sub(C)),
        0x92 => Ok(Sub(D)),
        0x93 => Ok(Sub(E)),
        0x94 => Ok(Sub(H)),
        0x95 => Ok(Sub(L)),
        0x96 => Ok(SubM),
        0x97 => Ok(Sub(A)),
        0x98 => Ok(Sbb(B)),
        0x99 => Ok(Sbb(C)),
        0x9a => Ok(Sbb(D)),
        0x9b => Ok(Sbb(E)),
        0x9c => Ok(Sbb(H)),
        0x9d => Ok(Sbb(L)),
        0x9e => Ok(SbbM),
        0x9f => Ok(Sbb(A)),
        0xa0 => Ok(Ana(B)),
        0xa1 => Ok(Ana(C)),
        0xa2 => Ok(Ana(D)),
        0xa3 => Ok(Ana(E)),
        0xa4 => Ok(Ana(H)),
        0xa5 => Ok(Ana(L)),
        0xa6 => Ok(AnaM),
        0xa7 => Ok(Ana(A)),
        0xa8 => Ok(Xra(B)),
        0xa9 => Ok(Xra(C)),
        0xaa => Ok(Xra(D)),
        0xab => Ok(Xra(E)),
        0xac => Ok(Xra(H)),
        0xad => Ok(Xra(L)),
        0xae => Ok(XraM),
        0xaf => Ok(Xra(A)),
        0xb0 => Ok(Ora(B)),
        0xb1 => Ok(Ora(C)),
        0xb2 => Ok(Ora(D)),
        0xb3 => Ok(Ora(E)),
        0xb4 => Ok(Ora(H)),
        0xb5 => Ok(Ora(L)),
        0xb6 => Ok(OraM),
        0xb7 => Ok(Ora(A)),
        0xb8 => Ok(Cmp(B)),
        0xb9 => Ok(Cmp(C)),
        0xba => Ok(Cmp(D)),
        0xbb => Ok(Cmp(E)),
        0xbc => Ok(Cmp(H)),
        0xbd => Ok(Cmp(L)),
        0xbe => Ok(CmpM),
        0xbf => Ok(Cmp(A)),
        0xc0 => Ok(Rnz),
        0xc1 => Ok(Pop(Reg16::B)),
        0xc2 => expect_data_u16(bytes).map(Jnz),
        0xc3 => expect_data_u16(bytes).map(Jmp),
        0xc4 => expect_data_u16(bytes).map(Cnz),
        0xc5 => Ok(Push(Reg16::B)),
        0xc6 => expect_data_byte(bytes).map(Adi),
        0xc7 => Ok(Rst(0)),
        0xc8 => Ok(Rz),
        0xc9 => Ok(Ret),
        0xca => expect_data_u16(bytes).map(Jz),

        0xcc => expect_data_u16(bytes).map(Cz),
        0xcd => expect_data_u16(bytes).map(Call),
        0xce => expect_data_byte(bytes).map(Aci),
        0xcf => Ok(Rst(1)),
        0xd0 => Ok(Rnc),
        0xd1 => Ok(Pop(Reg16::D)),
        0xd2 => expect_data_u16(bytes).map(Jnc),
        0xd3 => expect_data_byte(bytes).map(Out),
        0xd4 => expect_data_u16(bytes).map(Cnc),
        0xd5 => Ok(Push(Reg16::D)),
        0xd6 => expect_data_byte(bytes).map(Sui),
        0xd7 => Ok(Rst(2)),
        0xd8 => Ok(Rc),

        0xda => expect_data_u16(bytes).map(Jc),
        0xdb => expect_data_byte(bytes).map(In),
        0xdc => expect_data_u16(bytes).map(Cc),

        0xde => expect_data_byte(bytes).map(Sbi),
        0xdf => Ok(Rst(3)),
        0xe0 => Ok(Rpo),
        0xe1 => Ok(Pop(Reg16::H)),
        0xe2 => expect_data_u16(bytes).map(Jpo),
        0xe3 => Ok(Xthl),
        0xe4 => expect_data_u16(bytes).map(Cpo),
        0xe5 => Ok(Push(Reg16::H)),
        0xe6 => expect_data_byte(bytes).map(Ani),
        0xe7 => Ok(Rst(4)),
        0xe8 => Ok(Rpe),
        0xe9 => Ok(Pchl),
        0xea => expect_data_u16(bytes).map(Jpe),
        0xeb => Ok(Xchg),
        0xec => expect_data_u16(bytes).map(Cpe),

        0xee => expect_data_byte(bytes).map(Xri),
        0xef => Ok(Rst(5)),
        0xf0 => Ok(Rp),
        0xf1 => Ok(PopPSW),
        0xf2 => expect_data_u16(bytes).map(Jp),
        0xf3 => Ok(Di),
        0xf4 => expect_data_u16(bytes).map(Cp),
        0xf5 => Ok(PushPSW),
        0xf6 => expect_data_byte(bytes).map(Ori),
        0xf7 => Ok(Rst(6)),
        0xf8 => Ok(Rm),
        0xf9 => Ok(Sphl),
        0xfa => expect_data_u16(bytes).map(Jm),
        0xfb => Ok(Ei),
        0xfc => expect_data_u16(bytes).map(Cm),

        0xfe => expect_data_byte(bytes).map(Cpi),
        0xff => Ok(Rst(7)),
        _ => Ok(Nop),
    }).unwrap_or(Err(UnexpectedEndOfInput))
}

fn expect_data_u16(bytes: &[u8]) -> Result<u16, UnexpectedEndOfInput> {
    expect_2_data_bytes(bytes).map(|(a,b)| ((b as u16) << 8) | a as u16)
}

fn expect_2_data_bytes(bytes: &[u8]) -> Result<(u8, u8), UnexpectedEndOfInput> {
    bytes.get(1)
        .and_then(|a| bytes.get(2).map(|b| (*a, *b)))
        .ok_or(UnexpectedEndOfInput)
}

fn expect_data_byte(bytes: &[u8]) -> Result<u8, UnexpectedEndOfInput> {
    bytes.get(1).map(|a| *a).ok_or(UnexpectedEndOfInput)
}

pub struct OpCodes<U: BufRead> {
    reader: U
}

impl<U: BufRead> OpCodes<U> {

    pub fn new(reader: U) -> OpCodes<U> {
        OpCodes { reader }
    }
}

impl<U: BufRead> std::iter::Iterator for OpCodes<U> where U: std::io::Read{
    type Item = Result<OpCode, UnexpectedEndOfInput>;

    fn next(&mut self) -> Option<Result<OpCode, UnexpectedEndOfInput>> {
        let opcode_opt = match self.reader.fill_buf() {
            Ok([]) => None,
            Ok(buf) => Some(read_opcode(buf)),
            Err(_) => Some(Err(UnexpectedEndOfInput)),
        };
        
        match opcode_opt {
            Some(Ok(ref op)) => self.reader.consume(op.size() as usize),
            Some(Err(_)) => self.reader.consume(1),
            _ => {}
        }
        opcode_opt
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnexpectedEndOfInput;

impl fmt::Display for UnexpectedEndOfInput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,"{}", self.description())
    }
}

impl Error for UnexpectedEndOfInput {
    fn description(&self) -> &str {
        "unexpected end of input"
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}


#[cfg(test)]
mod tests {
    use opcode::read_opcode;
    use opcode::OpCode::*;
    use opcode::Register;
    use opcode::Reg16;

    #[test]
    fn read_opcodes() {
        let bytes = [0x13, 0xf3, 0x04, 0x87];
        assert_eq!(read_opcode(&bytes[0..]), Ok(Inx(Reg16::D)));
        assert_eq!(read_opcode(&bytes[1..]), Ok(Di));
        assert_eq!(read_opcode(&bytes[2..]), Ok(Inr(Register::B)));
        assert_eq!(read_opcode(&bytes[3..]), Ok(Add(Register::A)));
    }

    #[test]
    fn read_complex_opcodes() {
        assert_eq!(read_opcode(&[0xd6, 0xf3]), Ok(Sui(0xf3)));
        assert_eq!(read_opcode(&[0xd4, 0x87, 0x97]), Ok(Cnc(0x9787)));
    }
}