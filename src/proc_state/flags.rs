/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#[derive(Default, Debug, PartialEq)]
pub struct Flags {
   pub z: bool,
   pub s: bool,
   pub p: bool,
   pub cy: bool,
   pub ac: bool,
}

impl Flags {

    pub fn from_processor_status_word(psw: u8) -> Flags {
        Flags {
            z: (psw & (1 << 6)) != 0,
            s: (psw & (1 << 7)) != 0,
            p: (psw & (1 << 2)) != 0,
            cy: (psw & 1) != 0,
            ac: (psw & (1 << 4)) != 0,
        }
    }

    pub fn set_flags_for(&mut self, operand1: u8, result: i16) {
        self.z = (result as u8) == 0;
        self.s = (result & 0x0080) != 0;
        self.p = Flags::parity(result as u8);
        self.cy = (result as u16 & 0xff00) != 0;
        self.ac = (operand1 & 0x000f) + ((result.wrapping_sub(operand1 as i16)) & 0x000f) as u8 > 0x0f;
    }

    pub fn set_flags_except_carry_for(&mut self, operand1: u8, result: i16) {
        self.z = (result as u8) == 0;
        self.s = (result & 0x0080) != 0;
        self.p = Flags::parity(result as u8);
        self.ac = (operand1 & 0x000f) + ((result.wrapping_sub(operand1 as i16)) & 0x000f) as u8 > 0x0f;
    }

    pub fn to_processor_status_word(&self) -> u8 {
        (self.cy as u8)
            + 2
            + ((self.p as u8) << 2)
            + ((self.ac as u8) << 4)
            + ((self.z as u8) << 6)
            + ((self.s as u8) << 7)
    }

    fn parity(mut value: u8) -> bool {
        value ^= value >> 4;
        value ^= value >> 2;
        value ^= value >> 1;
        (value & 1) == 0
    }
}

#[cfg(test)]
mod tests {
  use super::Flags;

    #[test]
    fn flags_non_zero_sign_pair_ac() {
        let mut flags: Flags = Default::default();

        flags.set_flags_for(0x0f, 0x96);
        assert_eq!(flags.z, false);
        assert_eq!(flags.s, true);
        assert_eq!(flags.p, true);
        assert_eq!(flags.cy, false);
        assert_eq!(flags.ac, true);
    }

    #[test]
    fn flags_zero() {
        let mut flags: Flags = Default::default();

        flags.set_flags_for(0, 0);
        assert_eq!(flags.z, true);
        assert_eq!(flags.s, false);
        assert_eq!(flags.p, true);
        assert_eq!(flags.cy, false);
        assert_eq!(flags.ac, false);
    }

    #[test]
    fn flags_carry() {
        let mut flags: Flags = Default::default();

        flags.set_flags_for(0, 0x0101);
        assert_eq!(flags.z, false);
        assert_eq!(flags.s, false);
        assert_eq!(flags.p, false);
        assert_eq!(flags.cy, true);
        assert_eq!(flags.ac, false);
    }

    #[test]
    fn flags_underflow() {
        let mut flags: Flags = Default::default();

        flags.set_flags_for(0, -0x0012);
        assert_eq!(flags.z, false);
        assert_eq!(flags.s, true);
        assert_eq!(flags.p, true);
        assert_eq!(flags.cy, true);
        assert_eq!(flags.ac, false);
    }
    
}