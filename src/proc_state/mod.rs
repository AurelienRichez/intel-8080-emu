/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */


mod flags;
mod registers;

use std::fmt;

use opcode::{self, OpCode, Register, Reg16};
use opcode::OpCode::*;
pub use self::flags::Flags;
pub use self::registers::Registers;

/// Interface used by `Proc8080` for `IN` and `OUT` instructions. This is the main way to interact 
/// with the processor emulation.
/// 
/// The 8008 processor communicates with external devices via the instructions `IN` (the CPU reads 
/// from the databus on a given port) and `OUT` (the CPU writes to a given port). Reading and 
/// writing to the bus can do anything depending on the hardware (playing a sound, asking
/// specialized hardware to perform a computation, reading user keypresses...). 
/// 
/// This trait reify the  interactions with this data bus and is intended to be implemented by the 
/// user of the library in order to integrate with the emulator.
pub trait DataBus {

    /// Called by `Proc8080` when it applies a `IN` instruction
    fn read_port(&self, port: u8) -> u8;

    /// Called by `Proc8080` when it applies a `OUT` instruction
    fn write_port(&mut self, port: u8, value: u8);
    
}

/// Structure containing the processor state (flags, registers and memory) and logic.
pub struct Proc8080<Bus: DataBus> {
    flags: Flags,
    registers: Registers,
    memory: Box<[u8]>,
    cycles: u64,
    interupt_enabled: bool,
    stopped: bool,
    data_bus: Bus
}

impl<Bus: DataBus> Proc8080<Bus> {

    /// Builds a new `Proc8080` with the given memory `mem` and `DataBus`.
    /// 
    /// The memory contains both the ROM and RAM of the processor. Usually it is called with a
    /// boxed array of size 0xffff (adressable space for the intel 8080) which contains the ROM at
    /// the beginning of the memory. The processor starts with the program counter equal to 0.
    /// 
    /// The `data_bus` contains callbacks called when running a `IN` or `OUT` opcode.
    pub fn new(
            mem: Box<[u8]>, 
            data_bus: Bus
         ) -> Proc8080<Bus> {
        Proc8080 {
            flags: Default::default(), 
            registers: Default::default(),
            memory: mem,
            cycles: 0,
            interupt_enabled: false,
            stopped: false,
            data_bus,
        }
    }

    /// Borrows the procesor flags immutably.
    pub fn flags(&self) -> &Flags {
        &self.flags
    }

    /// Borrows the procesor registers immutably.
    pub fn registers(&self) -> &Registers {
        &self.registers
    }

    /// Reads the next opcode in memory and changes state accordingly.
    /// 
    /// This methods run one `step` of the processor simulation. It reads the next opcode according
    /// to the program counter
    pub fn emulate(&mut self) {
        if !self.stopped {
            let opcode = opcode::read_opcode(&self.memory[(self.registers.pc as usize)..])
                .unwrap_or_else(|_| { 
                    opcode::read_opcode_safe(&self.get_next_3_bytes())
                });
            self.registers.pc = self.registers.pc.wrapping_add(opcode.size());
            self.apply_op(opcode);
        }
    }

    /// Borrows the memory immutably.
    pub fn memory(&self) -> &[u8] {
        &self.memory
    }

    /// Make the processor run a `RST` instruction.
    /// 
    /// There are 8 possible `RST` instruction for the 8080 (`RST 0` to `7`). The specific 
    /// instruction is chosen via `rst_value`.
    /// 
    /// An interrupt restart a processor which was in stopped state after running a HLT opcode.
    /// 
    /// # Panics 
    /// If `rst_value` is greater than 7;
    pub fn interrupt(&mut self, rst_value: u8) {
        assert!(rst_value <= 7, "RST value are only from 0 to 7");
        self.stopped = false;
        if self.interupt_enabled { 
            self.interupt_enabled = false;
            self.apply_op(Rst(rst_value));
        }
    }

    /// Cycles elapsed since the cpu was created
    pub fn cycles(&self) -> u64 {
        self.cycles
    }

    fn get_next_3_bytes(&mut self) -> [u8;3] {
        let mut bytes = [0;3];
        for i in 0..3 {
            bytes[i] = self.memory[self.registers.pc as usize];
            self.registers.pc += self.registers.pc.wrapping_add(1);
        }
        bytes
    }
    
    fn apply_op(&mut self, op: OpCode) {
        match op {
            Nop => (),

            // Data transfer
            Mov(reg1, reg2) => self.registers.apply_mov(reg1, reg2),
            MovFromM(reg) => self.move_from_mem(reg),
            MovToM(reg) => self.move_to_mem(reg),
            Mvi(reg, value) => self.registers.set_reg_val(reg, value),
            MviM(value) => self.memory[self.registers.reg_16_val(Reg16::H) as usize] = value,
            Lxi(reg, value) => self.registers.set_reg_16_val(reg, value),
            Lda(addr) => self.registers.set_reg_val(Register::A, self.memory[addr as usize]),
            Sta(addr) => self.memory[addr as usize] = self.registers.reg_val(Register::A),
            Lhld(addr) => self.lhld(addr),
            Shld(addr) => self.shld(addr),
            LdaxB => self.ldax(Reg16::B),
            LdaxD => self.ldax(Reg16::D),
            StaxB => self.stax(Reg16::B),
            StaxD => self.stax(Reg16::D),
            Xchg => self.xchg(),

            // Arithmetic
            Add(reg) => self.add_reg(reg),
            AddM => self.add_from_mem(),
            Adi(value) => self.add_to_accumulator(value as i16),
            Adc(reg) => self.add_reg_with_carry(reg),
            AdcM =>  self.add_from_mem_with_borrow(),
            Aci(value) => self.add_immediate_with_carry(value),
            Sub(reg) => self.sub_reg(reg),
            SubM => self.sub_from_mem(),
            Sui(value) => self.add_to_accumulator(-(value as i16)),
            Sbb(reg) => self.sub_reg_with_borrow(reg),
            SbbM => self.sub_from_mem_with_borrow(),
            Sbi(value) => self.sub_immediate_with_borrow(value),
            Inr(reg) => self.increment_register(reg),
            InrM => self.increment_memory(),
            Dcr(reg) => self.decrement_register(reg),
            DcrM => self.decrement_memory(),
            Inx(reg) => self.increment_reg_pair(reg),
            Dcx(reg) => self.decrement_reg_pair(reg),
            Dad(reg) => self.add_register_pair_to_h(reg),
            Daa => self.decimal_adjust_accumulator(),

            // Logical
            Ana(reg) => self.and_register(reg),
            AnaM => self.and_memory(),
            Ani(value) => self.and_immediate(value),
            Xra(reg) => self.xor_register(reg),
            XraM => self.xor_memory(),
            Xri(value) => self.apply_xor(value),
            Ora(reg) => self.or_register(reg),
            OraM => self.or_memory(),
            Ori(value) => self.apply_or(value),
            Cmp(reg) => self.compare_reg(reg),
            CmpM => self.compare_mem(),
            Cpi(value) => self.compare(value),
            Rlc => self.rotate_left(),
            Rrc => self.rotate_right(),
            Ral => self.rotate_left_through_carry(),
            Rar => self.rotate_right_through_carry(),
            Cma => self.complement(),
            Cmc => self.flags.cy = !self.flags.cy,
            Stc => self.flags.cy = true,

            // Branch
            Jmp(addr) => self.registers.pc = addr,
            Jnz(addr) => if !self.flags.z  { self.registers.pc = addr },
            Jz(addr) => if self.flags.z  { self.registers.pc = addr },
            Jnc(addr) => if !self.flags.cy  { self.registers.pc = addr },
            Jc(addr) => if self.flags.cy  { self.registers.pc = addr },
            Jpo(addr) => if !self.flags.p  { self.registers.pc = addr },
            Jpe(addr) => if self.flags.p  { self.registers.pc = addr },
            Jp(addr) => if !self.flags.s  { self.registers.pc = addr },
            Jm(addr) => if self.flags.s  { self.registers.pc = addr },
            Call(addr) => self.apply_call(addr),
            Cnz(addr) => if !self.flags.z { self.apply_call(addr) } ,
            Cz(addr) => if self.flags.z { self.apply_call(addr) } ,
            Cnc(addr) => if !self.flags.cy { self.apply_call(addr) } ,
            Cc(addr) => if self.flags.cy { self.apply_call(addr) } ,
            Cpo(addr) => if !self.flags.p { self.apply_call(addr) } ,
            Cpe(addr) => if self.flags.p { self.apply_call(addr) } ,
            Cp(addr) => if !self.flags.s { self.apply_call(addr) } ,
            Cm(addr) => if self.flags.s { self.apply_call(addr) } ,
            Ret => self.apply_return(),
            Rnz => if !self.flags.z { self.apply_return() } ,
            Rz => if self.flags.z { self.apply_return() } ,
            Rnc => if !self.flags.cy { self.apply_return() } ,
            Rc => if self.flags.cy { self.apply_return() } ,
            Rpo => if !self.flags.p { self.apply_return() } ,
            Rpe => if self.flags.p { self.apply_return() } ,
            Rp => if !self.flags.s { self.apply_return() } ,
            Rm => if self.flags.s { self.apply_return() } ,
            Rst(value) => self.apply_call((value * 8) as u16),
            Pchl => {
                let addr = self.registers.reg_16_val(Reg16::H);
                self.registers.pc = addr;
            },

            // Stack, I/O, and Machine Control
            Push(reg) => self.push_reg(reg),
            PushPSW => self.push_processor_status_word(),
            Pop(reg) => self.pop_register(reg),
            PopPSW => self.pop_processor_status_word(),
            Xthl => self.xthl(),
            Sphl => self.registers.sp = self.registers.reg_16_val(Reg16::H),
            In(port) => self.registers.set_reg_val(Register::A, self.data_bus.read_port(port)),
            Out(port) => self.data_bus.write_port(port, self.registers.reg_val(Register::A)),
            Ei => self.interupt_enabled = true,
            Di => self.interupt_enabled = false,
            Hlt => self.stopped = false,
        }
        self.cycles += op.cycles();
    }

    fn move_to_mem(&mut self, reg: Register) {
        self.memory[self.registers.reg_16_val(Reg16::H) as usize] = self.registers.reg_val(reg);
    }

    fn move_from_mem(&mut self, reg: Register) {
        let mem_value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        self.registers.set_reg_val(reg, mem_value);
    }

    fn lhld(&mut self, addr: u16) {
        self.registers.set_reg_val(Register::H, self.memory[(addr + 1) as usize]);
        self.registers.set_reg_val(Register::L, self.memory[addr as usize]);
    }
    
    fn shld(&mut self, addr: u16) {
        self.memory[addr as usize] = self.registers.reg_val(Register::L);
        self.memory[(addr + 1) as usize] = self.registers.reg_val(Register::H);
    }

    fn ldax(&mut self, reg: Reg16) { 
        let value = self.memory[self.registers.reg_16_val(reg) as usize];
        self.registers.set_reg_val(Register::A, value);
    }

    fn stax(&mut self, reg: Reg16) {
        let addr = self.registers.reg_16_val(reg) as usize;
        self.memory[addr] = self.registers.reg_val(Register::A);
    }

    fn xchg(&mut self) {
        let d = self.registers.reg_16_val(Reg16::D);
        let h = self.registers.reg_16_val(Reg16::H);

        self.registers.set_reg_16_val(Reg16::D, h);
        self.registers.set_reg_16_val(Reg16::H, d);
    }

    fn add_reg(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        self.add_to_accumulator(value as i16);
    }

    fn add_from_mem(&mut self) {
        let value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        self.add_to_accumulator(value as i16);
    }

    fn add_from_mem_with_borrow(&mut self) {
        let value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        let carry = self.flags().cy as u8;

        self.add_to_accumulator(value.wrapping_add(carry) as i16);
    }

    fn add_reg_with_carry(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        let carry = self.flags().cy as u8;
        self.add_to_accumulator(value.wrapping_add(carry) as i16);
    }

    fn add_immediate_with_carry(&mut self, value: u8) {
        let carry = self.flags().cy as u8;
        self.add_to_accumulator(value.wrapping_add(carry) as i16);
    }

    fn sub_reg(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        self.add_to_accumulator(-(value as i16));
    }

    fn sub_from_mem(&mut self) {
        let value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        self.add_to_accumulator(-(value as i16));
    }

    fn sub_reg_with_borrow(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        let carry = self.flags().cy as u8;
        self.add_to_accumulator(-(value.wrapping_add(carry) as i16));
    }

    fn sub_from_mem_with_borrow(&mut self) {
        let value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        let carry = self.flags().cy as u8;

        self.add_to_accumulator(-(value.wrapping_add(carry) as i16));
    }

    fn sub_immediate_with_borrow(&mut self, value: u8) {
        let carry = self.flags().cy as u8;
        self.add_to_accumulator(-(value.wrapping_add(carry) as i16));
    }

    fn add_to_accumulator(&mut self, value: i16) {
        let a = self.registers.reg_val(Register::A);
        let result: i16 = (a as i16).wrapping_add(value) ;

        self.registers.set_reg_val(Register::A, result as u8);
        self.flags.set_flags_for(a, result);
    }

    fn increment_register(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        let result = (value as i16) + 1;

        self.registers.set_reg_val(reg, result as u8);
        self.flags.set_flags_except_carry_for(value, result as i16);
    }

    fn increment_memory(&mut self) {
        let index = self.registers.reg_16_val(Reg16::H) as usize;
        let result = self.memory[index].wrapping_add(1);
        self.memory[index] = result;
        self.flags.set_flags_except_carry_for(result, result as i16);
    }

    fn decrement_register(& mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        let result = (value as i16).wrapping_sub(1);

        self.registers.set_reg_val(reg, result as u8);
        self.flags.set_flags_except_carry_for(value, result);
    }

    fn decrement_memory(&mut self) {
        let index = self.registers.reg_16_val(Reg16::H) as usize;
        let result = self.memory[index].wrapping_sub(1);
        self.memory[index] = result;
        self.flags.set_flags_except_carry_for(result, result as i16);
    }

    fn increment_reg_pair(&mut self, reg: Reg16) {
        let value = self.registers.reg_16_val(reg);
        let result = value.wrapping_add(1);
        self.registers.set_reg_16_val(reg, result);
    }

    fn decrement_reg_pair(&mut self, reg: Reg16) {
        let value = self.registers.reg_16_val(reg);
        let result = value.wrapping_sub(1);
        self.registers.set_reg_16_val(reg, result);
    }

    fn add_register_pair_to_h(&mut self, reg: Reg16) {
        let h = self.registers.reg_16_val(Reg16::H) as u32;
        let value = self.registers.reg_16_val(reg) as u32;
        let result = h + value;

        self.registers.set_reg_16_val(Reg16::H, result as u16);
        self.flags.cy = result > 0xffff
    }

    fn decimal_adjust_accumulator(&mut self) {
        let low_correction = self.low_decimal_adjustment();
        let high_correction = self.high_decimal_adjustment(low_correction);
        self.add_to_accumulator(low_correction + high_correction)
    }

    fn low_decimal_adjustment(&self) -> i16 {
        let a = self.registers.reg_val(Register::A) as i16;
        if self.flags.ac || a & 0x0f > 9 {
            0x06
        } else { 
            0x00 
        }
    }

    fn high_decimal_adjustment(&self, low_correction: i16) -> i16 {
        let a = self.registers.reg_val(Register::A) as i16;
        if self.flags.cy  || a + low_correction > 0x90 {
            0x60
        } else { 
            0x00
        }
    }

    fn and_register(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        self.apply_and(value);
    }

    fn and_memory(&mut self) {
        let value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        self.apply_and(value);
    }

    fn and_immediate(&mut self, value: u8) {
        self.apply_and(value);
        self.flags.ac = false;
    } 

    fn apply_and(&mut self, value: u8) {
        let a = self.registers.reg_val(Register::A);
        let result = value & a;

        self.registers.set_reg_val(Register::A, result);
        self.flags.set_flags_for(a, result as i16);
    }

    fn xor_register(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        self.apply_xor(value);
    }

    fn xor_memory(&mut self) {
        let value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        self.apply_xor(value);
    }

    fn apply_xor(&mut self, value: u8) {
        let a = self.registers.reg_val(Register::A);
        let result = a ^ value;
        self.registers.set_reg_val(Register::A, result);
        self.flags.set_flags_for(a, result as i16);
        self.flags.ac = false;
    }

    fn or_register(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        self.apply_or(value);
    }

    fn or_memory(&mut self) {
        let value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        self.apply_or(value);
    }

    fn apply_or(&mut self, value: u8) {
        let a = self.registers.reg_val(Register::A);
        let result = a | value;
        self.registers.set_reg_val(Register::A, result);
        self.flags.set_flags_for(a, result as i16);
        self.flags.ac = false;
    }

    fn compare_reg(&mut self, reg: Register) {
        let value = self.registers.reg_val(reg);
        self.compare(value);
    }

    fn compare_mem(&mut self) {
        let value = self.memory[self.registers.reg_16_val(Reg16::H) as usize];
        self.compare(value);
    }

    fn compare(&mut self, value: u8) {
        let a = self.registers.reg_val(Register::A);
        self.flags.set_flags_for(a, a as i16 - value as i16);
    }

    fn rotate_left(&mut self) {
        let a = self.registers.reg_val(Register::A);
        self.registers.set_reg_val(Register::A, a.rotate_left(1));
        self.flags.cy = (a & 0x80) != 0;
    }

    fn rotate_right(&mut self) {
        let a = self.registers.reg_val(Register::A);
        self.registers.set_reg_val(Register::A, a.rotate_right(1));
        self.flags.cy = (a & 0x01) != 0;
    }

    fn rotate_left_through_carry(&mut self) {
        let a = self.registers.reg_val(Register::A);
        let carry = self.flags.cy as u8;
        self.registers.set_reg_val(Register::A, (a << 1) + carry);
        self.flags.cy = (a & 0x80) != 0;
    }

    fn rotate_right_through_carry(&mut self) {
        let a = self.registers.reg_val(Register::A);
        let carry = (self.flags.cy as u8) << 7;
        self.registers.set_reg_val(Register::A, (a >> 1) + carry);
        self.flags.cy = (a & 0x01) != 0;
    }

    fn complement(&mut self) {
        let a = self.registers.reg_val(Register::A);
        self.registers.set_reg_val(Register::A, !a);
    }

    fn apply_call(&mut self, addr: u16) {
        let pc = self.registers.pc;
        self.push_stack(pc);
        self.registers.pc = addr;
    }

    fn apply_return(&mut self) {
        let sp = self.registers.sp;
        let low = self.memory[sp as usize];
        let high = self.memory[sp.wrapping_add(1) as usize];
        self.registers.sp = sp.wrapping_add(2);
        self.registers.pc = ((high as u16) << 8) + low as u16;
    }

    fn push_reg(&mut self, reg: Reg16) {
        let value = self.registers.reg_16_val(reg);
        self.push_stack(value);
    }

    fn push_stack(&mut self, value: u16) {
        let new_stack_pointer = self.registers.sp.wrapping_sub(2);
        self.set_mem16(new_stack_pointer, value);
        self.registers.sp = new_stack_pointer;
    }

    fn pop_stack(&mut self) -> u16 {
        let sp = self.registers.sp;
        let value = self.get_mem16(sp);
        self.set_mem16(sp, value);
        self.registers.sp = sp.wrapping_add(2);
        value
    }

    fn set_mem16(&mut self, addr: u16, value: u16) {
        let low = value as u8;
        let high = (value >> 8) as u8;
        self.memory[addr as usize] = low;
        self.memory[addr.wrapping_add(1) as usize] = high;
    }

    fn get_mem16(&self, addr: u16) -> u16 {
        self.memory[addr as usize] as u16
         + ((self.memory[addr.wrapping_add(1) as usize] as u16) << 8) 
    }


    fn push_processor_status_word(&mut self) {
        let value = ((self.registers.reg_val(Register::A) as u16) << 8) + self.flags.to_processor_status_word() as u16;
        self.push_stack(value);
    }

    fn pop_register(&mut self, reg: Reg16) {
        let value = self.pop_stack();
        self.registers.set_reg_16_val(reg, value);
    }

    fn pop_processor_status_word(&mut self) {
        let value = self.pop_stack();
        self.registers.set_reg_val(Register::A, (value >> 8) as u8);
        self.flags = Flags::from_processor_status_word(value as u8);
    }

    fn xthl(&mut self) {
        let addr = self.registers.sp;
        let h_value = self.registers.reg_16_val(Reg16::H);
        let mem_value = self.get_mem16(addr);
        self.set_mem16(addr, h_value);
        self.registers.set_reg_16_val(Reg16::H, mem_value);
    }
}

impl<Bus: DataBus> fmt::Debug for Proc8080<Bus> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{
    flags: {:x?},
    registers: {:x?},
    interupt_enabled: {:?},
}}", self.flags, self.registers, self.interupt_enabled)
    }
}

/// A simple wrapper around [`Proc8080`](struct.Proc8080.html) which allows to intercept opcodes.
/// 
/// This structure is useful for debugging, fixing some missing components like a call to code which
/// does not exist in the rom etc. It has the same interface as `Proc8080`.
pub struct InterceptableProc8080<Bus: DataBus, Intercept: Fn(&Proc8080<Bus>, &OpCode) -> bool> {
    proc8080: Proc8080<Bus>,
    interceptor: Intercept,
}

impl<Bus, Intercept> InterceptableProc8080<Bus, Intercept> 
    where Bus: DataBus, 
        Intercept: Fn(&Proc8080<Bus>, &OpCode) -> bool {

    /// Builds an `InterceptableProc8080` by taking ownership of an exiting `Proc8080`
    /// 
    /// `interceptor` is a closure which allows to run arbitrary code before the actual emulator
    /// apply an opcode, and after the program counter is increased (i.e. `proc.registers().pc` will 
    /// point to the next instruction after the current one). The interceptor can then do wathever 
    /// it  needs to and must then returns : 
    ///  - `true` if the processor should go on and apply the opcode normally
    ///  - `false` if it should ignore it and skip to the next instruction
    pub fn from_8080(proc8080: Proc8080<Bus>, interceptor: Intercept) -> InterceptableProc8080<Bus, Intercept> {
        InterceptableProc8080 { proc8080, interceptor }
    }

    /// Takes ownership of the wrapped `Proc8080`. 
    pub fn unwrap(self) -> Proc8080<Bus> {
        self.proc8080
    }

    pub fn emulate(&mut self) -> Result<(), opcode::UnexpectedEndOfInput> {
        opcode::read_opcode(&self.proc8080.memory[(self.proc8080.registers.pc as usize)..])
            .map(|op| { 
                self.proc8080.registers.pc = self.proc8080.registers.pc.wrapping_add(op.size());
                if !(self.interceptor)(&self.proc8080, &op) {
                    self.proc8080.apply_op(op);
                } 
            })
    }

    pub fn flags(&self) -> &Flags {
        self.proc8080.flags()
    }

    pub fn registers(&self) -> &Registers {
        self.proc8080.registers()
    }

    pub fn memory(&self) -> &[u8] {
        self.proc8080.memory()
    }

    pub fn interrupt(&mut self, rst_value: u8) {
        self.proc8080.interrupt(rst_value)
    }

    /// Cycles elapsed since the cpu was created
    pub fn cycles(&self) -> u64 {
        self.proc8080.cycles()
    }
}

#[cfg(test)]
mod tests {
    use::proc_state::*;
    use opcode::Register::*;

    pub struct DummyBus {}

    impl DataBus for DummyBus {
        fn read_port(&self, _port: u8) -> u8 { 0 }

        fn write_port(&mut self, _port: u8, _value: u8) {}
    }

    struct ProcFixture {
        proc8080: Proc8080<DummyBus>
    }

    impl ProcFixture {
        
        pub fn new() -> ProcFixture {
            ProcFixture {
                proc8080: Proc8080::new(Box::new([0x00; 0xffff]), DummyBus {}),
            }
        }

        pub fn with_regs(mut self, registers: Registers) -> ProcFixture {
            self.proc8080.registers = registers;
            self
        }

        pub fn with_reg(mut self, reg: Register, value: u8) -> ProcFixture {
            self.proc8080.registers.set_reg_val(reg, value);
            self
        }

        pub fn with_mem_at_index(mut self, index: u16, chunk: &[u8]) -> ProcFixture {
            let idx = index as usize;
            self.proc8080.memory[idx..(idx + chunk.len())].clone_from_slice(chunk);
            self
        }

        pub fn with_carry(mut self, state: bool) -> ProcFixture {
            self.proc8080.flags.cy = state;
            self
        }

        pub fn with_flags(mut self, flags: Flags) -> ProcFixture {
            self.proc8080.flags = flags;
            self
        }

        pub fn with_op(mut self, op: OpCode) -> ProcFixture {
            self.proc8080.apply_op(op);
            self
        }

        pub fn with_sp(mut self, value: u16) -> ProcFixture {
            self.proc8080.registers.sp = value;
            self
        }

        pub fn with_pc(mut self, value: u16) -> ProcFixture {
            self.proc8080.registers.pc = value;
            self
        }

        pub fn should_have_mem(self, index: usize, expected_value: u8) -> ProcFixture {
            assert_eq!(
                self.proc8080.memory[index], expected_value, 
                "Memory address 0x{:x?} is 0x{:x?} instead of 0x{:x?}", 
                index, self.proc8080.memory[index], expected_value
            );
            self
        }
   
        pub fn should_have_reg(self, reg: Register, value: u8) -> ProcFixture {
            let actual_value = self.proc8080.registers().reg_val(reg);
            assert_eq!(
                actual_value, value,
                "register {} is 0x{:x?} instead of 0x{:x?}", reg , actual_value, value
             );
            self
        }

        pub fn should_have_reg16(self, reg: Reg16, value: u16) -> ProcFixture {
            let actual_value = self.proc8080.registers().reg_16_val(reg);
            assert_eq!(
                actual_value, value,
                "register {} is 0x{:x?} instead of 0x{:x?}", reg , actual_value, value
             );
            self
        }

        pub fn should_have_carry_equal_to(self, b: bool) -> ProcFixture {
            assert_eq!(
                self.proc8080.flags.cy, b,
                "Carry flag was not {}", b
            );
            self
        }

        pub fn should_have_pc_equal_to(self, value: u16) -> ProcFixture {
            assert_eq!(
                self.proc8080.registers.pc, value,
                "Program counter was not 0x{:x?}", value
            );
            self
        }

        pub fn should_have_sp_equal_to(self, value: u16) -> ProcFixture {
            assert_eq!(
                self.proc8080.registers.sp, value,
                "Stack pointer is 0x{:x?} instead of 0x{:x?}", 
                self.proc8080.registers.sp,value
            );
            self
        }

        pub fn unwrap(self) -> Proc8080<DummyBus> {
            self.proc8080
        }
    }


    #[test]
    fn mov_to_register() {
        ProcFixture::new()
            .with_regs(Registers { c: 0x15, a: 0xf2, ..Default::default() })
            .with_op(Mov(A, C))
            .should_have_reg(A, 0x15);
    }

    #[test]
    fn mov_from_memory() {
        ProcFixture::new()
            .with_regs(Registers { h: 0x00, l: 0x02, ..Default::default() })
            .with_mem_at_index(0, &[0x00, 0x00 , 0xf2, 0xf4])
            .with_op(MovFromM(A))
            .should_have_reg(A, 0xf2);
    }

    #[test]
    fn mov_to_memory() {
        ProcFixture::new()
            .with_regs(Registers { a: 0xff, h: 0x02, l: 0x0a, ..Default::default() } )
            .with_op(MovToM(A))
            .should_have_mem(0x020a, 0xff);
    }

    #[test]
    fn mov_immediate_register() {
        ProcFixture::new()
            .with_op(Mvi(C, 0xa1))
            .should_have_reg(C, 0xa1);
    }

    #[test]
    fn mov_immediate_memory() {
        ProcFixture::new()
            .with_regs(Registers { a: 0xff, h: 0x02, l: 0x0a, ..Default::default() })
            .with_op(MviM(0xa4))
            .should_have_mem(0x020a, 0xa4);
    }

    #[test]
    fn lxi() {
        ProcFixture::new()
            .with_op(Lxi(Reg16::H, 0xf1ff))
            .should_have_reg(H, 0xf1)
            .should_have_reg(L, 0xff)
            .should_have_reg16(Reg16::H, 0xf1ff);
    }

    #[test]
    fn lda() {
        ProcFixture::new()
            .with_mem_at_index(0x00fa, &[0x11])
            .with_op(Lda(0x00fa))
            .should_have_reg(A, 0x11);
    }

    #[test]
    fn sta() {
        ProcFixture::new()
            .with_reg(A, 0xc2)
            .with_op(Sta(0x00fa))
            .should_have_mem(0x00fa, 0xc2);
    }

    #[test]
    fn lhld() {
        ProcFixture::new()
            .with_mem_at_index(0x3450, &[0xf5, 0xc1])
            .with_op(Lhld(0x3450))
            .should_have_reg16(Reg16::H, 0xc1f5);
    }

    #[test]
    fn shld() {
        ProcFixture::new()
            .with_regs(Registers { l: 0x12, h: 0xd4, ..Default::default() })
            .with_op(Shld(0x00f1))
            .should_have_mem(0x00f1, 0x12)
            .should_have_mem(0x00f2, 0xd4);
    }

    #[test]
    fn ldax_b() {
        ProcFixture::new()
            .with_regs(Registers { b: 0x01, c: 0x10, ..Default::default() })
            .with_mem_at_index(0x0110, &[0xfa])
            .with_op(LdaxB)
            .should_have_reg(A, 0xfa);
    }

    #[test]
    fn ldax_d() {
        ProcFixture::new()
            .with_regs(Registers { d: 0x01, e: 0x10, ..Default::default() })
            .with_mem_at_index(0x0110, &[0xfa])
            .with_op(LdaxD)
            .should_have_reg(A, 0xfa);
    }

    #[test]
    fn stax_b() {
        ProcFixture::new()
            .with_regs(Registers { a: 0xef, b: 0xe1, c: 0x10, ..Default::default() })
            .with_op(StaxB)
            .should_have_mem(0xe110, 0xef);
    }

    #[test]
    fn stax_d() {
        ProcFixture::new()
            .with_regs(Registers { a: 0xef, d: 0xe1, e: 0x10, ..Default::default() })
            .with_op(StaxD)
            .should_have_mem(0xe110, 0xef);
    }

    #[test]
    fn xchg() {
        ProcFixture::new()
            .with_regs(Registers { h: 0xef, l: 0xe1, d: 0x00, e: 0x10, ..Default::default() })
            .with_op(Xchg)
            .should_have_reg(H, 0x00)
            .should_have_reg(L, 0x10)
            .should_have_reg(D, 0xef)
            .should_have_reg(E, 0xe1);
    }

    #[test]
    fn add_register() {
        ProcFixture::new()
            .with_regs(Registers { a: 0x48, d: 0x4e, ..Default::default()})
            .with_op(Add(D))
            .should_have_reg(A, 0x96);
    }

    #[test]
    fn add_memory() {
        ProcFixture::new()
            .with_regs(Registers { a: 0x12, h: 0x00, l: 0x02, ..Default::default() })
            .with_mem_at_index(0, &[0x00, 0x00 , 0xf2, 0xf4])
            .with_op(AddM)
            .should_have_reg(A, 0x04);
    }

    #[test]
    fn add_immediate() {
        ProcFixture::new()
            .with_reg(A, 0x48)
            .with_op(Adi(0x03))
            .should_have_reg(A, 0x4b);
    }

    #[test]
    fn add_reg_with_carry() {
        ProcFixture::new()
            .with_regs(Registers { a: 0x48, h: 0x02, ..Default::default()})
            .with_flags(Flags { cy: true, ..Default::default()})
            .with_op(Adc(H))
            .should_have_reg(A, 0x4b);;
    }

    #[test]
    fn add_mem_with_carry() {
        ProcFixture::new()
            .with_regs(Registers { a: 0x12, h: 0x00, l: 0x02, ..Default::default() })
            .with_mem_at_index(0, &[0x00, 0x00 , 0xf2, 0xf4])
             .with_flags(Flags { cy: true, ..Default::default()})
             .with_op(AdcM)
             .should_have_reg(A, 0x05);
    }

    #[test]
    fn add_immediate_with_carry_true() {
        ProcFixture::new().with_reg(A, 0x48)
            .with_flags(Flags { cy: true, ..Default::default()})
            .with_op(Aci(0x03))
            .should_have_reg(A, 0x4c);
    }

    #[test]
    fn add_immediate_with_carry_false() {
        ProcFixture::new().with_reg(A, 0x48)
            .with_flags(Flags { cy: false, ..Default::default()})
            .with_op(Aci(0x03))
            .should_have_reg(A, 0x4b);
    }

    #[test]
    fn add_immediate_with_carry_overflow() {
        ProcFixture::new().with_reg(A, 0x01)
            .with_flags(Flags { cy: true, ..Default::default()})
            .with_op(Aci(0xff))
            .should_have_reg(A, 0x01);
    }

    #[test]
    fn sub_register() {
        ProcFixture::new()
            .with_regs(Registers { a: 0x48, h: 0x02, ..Default::default()})
            .with_op(Sub(H))
            .should_have_reg(A, 0x46);
    }

    #[test]
    fn sub_memory() {
        ProcFixture::new()
            .with_regs(Registers { a: 0xf3, h: 0xff, l: 0x02, ..Default::default() })
            .with_mem_at_index(0xff00, &[0x00, 0x00 , 0xf2, 0xf4])
            .with_op(SubM)
            .should_have_reg(A, 0x01);
    }

    #[test]
    fn substract_immediate() {
        ProcFixture::new()
            .with_reg(A, 0x48)
            .with_op(Sui(0x13))
            .should_have_reg(A, 0x35);
    }

    #[test]
    fn substract_reg_with_borrow() {
        ProcFixture::new()
            .with_regs(Registers { a: 0x48, h: 0x02, ..Default::default()})
            .with_flags(Flags { cy: true, ..Default::default()})
            .with_op(Sbb(H))
            .should_have_reg(A, 0x45);
    }

    #[test]
    fn substract_mem_with_borrow() {
        ProcFixture::new()
            .with_regs(Registers { a: 0x03, h: 0x00, l: 0x02, ..Default::default() })
            .with_mem_at_index(0, &[0x00, 0x00 , 0x02, 0xf4])
            .with_flags(Flags { cy: true, ..Default::default()})
            .with_op(SbbM)
            .should_have_reg(A, 0x00);
    }

    #[test]
    fn substract_immediate_with_borrow() {
        ProcFixture::new().with_reg(A, 0x48)
            .with_flags(Flags { cy: true, ..Default::default()})
            .with_op(Sbi(0x03))
            .should_have_reg(A, 0x44);
    }

    #[test]
    fn increment_register_should_add_1() {
        ProcFixture::new()
            .with_reg(D, 0x0f)
            .with_op(Inr(D))
            .should_have_reg(D, 0x10);
    }

    #[test]
    fn increment_register_should_change_flags() {
        let proc8080 = ProcFixture::new()
            .with_reg(D, 0x0f)
            .with_op(Inr(D))
            .unwrap();
        // TODO find a way to be more consistent ? 
        let flags = proc8080.flags();
        assert_eq!(flags.z, false);
        assert_eq!(flags.s, false);
        assert_eq!(flags.p, false);
        assert_eq!(flags.ac, true);
    }

    #[test]
    fn increment_memory_should_add_1() {
        ProcFixture::new()
            .with_regs(Registers { h: 0x00, l: 0x02, ..Default::default() })
            .with_mem_at_index(0, &[0x00, 0x00 , 0x02, 0xf4])
            .with_op(InrM)
            .should_have_mem(0x02, 0x03);
    }

    #[test]
    fn decrement_register_should_substract_1() {
        ProcFixture::new()
            .with_reg(D, 0x0f)
            .with_op(Dcr(D))
            .should_have_reg(D, 0x0e);
    }

    #[test]
    fn decrement_memory_should_substract_1() {
        ProcFixture::new()
            .with_regs(Registers { h: 0x00, l: 0x02, ..Default::default() })
            .with_mem_at_index(0, &[0x00, 0x00 , 0x00, 0xf4])
            .with_op(DcrM)
            .should_have_mem(0x02, 0xff);
    }

    #[test]
    fn increment_register_pair() {
        ProcFixture::new()
            .with_reg(D, 0x0f)
            .with_op(Inx(Reg16::D))
            .should_have_reg16(Reg16::D, 0x00f01);
    }

    #[test]
    fn decrement_register_pair() {
        ProcFixture::new()
            .with_reg(H, 0x0f)
            .with_op(Dcx(Reg16::H))
            .should_have_reg16(Reg16::H, 0x00eff);
    }

    #[test]
    fn add_reg_to_hl() {
        ProcFixture::new()
            .with_regs(Registers { h: 0x0f, l: 0x11, d: 0x01, e: 0x01, ..Default::default()})
            .with_op(Dad(Reg16::D))
            .should_have_reg16(Reg16::H, 0x1012);
    }

    #[test]
    fn add_reg_to_hl_must_set_cy() {
        ProcFixture::new()
            .with_regs(Registers { h: 0xff, l: 0xff, d: 0x01, e: 0x01, ..Default::default()})
            .with_op(Dad(Reg16::D))
            .should_have_carry_equal_to(true);
    }

    #[test]
    fn decimal_adjust_accumulator_low() {
        ProcFixture::new()
            .with_reg(A, 0x4f)
            .with_op(Daa)
            .should_have_reg(A, 0x55);
    }

    #[test]
    fn decimal_adjust_accumulator_with_ac_flag() {
        ProcFixture::new()
            .with_reg(A, 0x42)
            .with_flags(Flags { ac: true, ..Default::default()})
            .with_op(Daa)
            .should_have_reg(A, 0x48);
    }

    #[test]
    fn test_binary_arithmetic() {
        ProcFixture::new()
            .with_op(Adi(0x19))
            .with_op(Adi(0x02))
            .with_op(Daa)
            .should_have_reg(A, 0x21)
            
            .with_op(Adi(0x90))
            .with_op(Daa)
            .should_have_reg(A, 0x11);
    }

    #[test]
    fn and_register_should_work() {
        ProcFixture::new()
            .with_regs(Registers { a: 0b01101110, b: 0b01101001, ..Default::default()})
            .with_op(Ana(B))
            .should_have_reg(A, 0b01101000);
    }

    #[test]
    fn and_register_should_affect_flags() {
        let mut proc8080 = ProcFixture::new()
        .with_regs(Registers { a: 0b11101110, b: 0b11101001, ..Default::default()})
        .with_op(Ana(B))
        .unwrap();
        // TODO find a way to be more consistent for flags
        {
            let flags = proc8080.flags();
            assert_eq!(flags.z, false);
            assert_eq!(flags.s, true);
            assert_eq!(flags.p, true);
            assert_eq!(flags.cy, false);
        }

        proc8080.apply_op(Ana(H));

        assert_eq!(proc8080.flags().z, true);
    }

    #[test]
    fn and_memory() {
        ProcFixture::new()
            .with_regs(Registers { a: 0b01101110, h: 0x00, l: 0x02, ..Default::default() })
            .with_mem_at_index(0, &[0x00, 0x00 , 0b11101001, 0xf4])
            .with_op(AnaM)
            .should_have_reg(A, 0b01101000);
    }

    #[test]
    fn and_immediate() {
        ProcFixture::new()
            .with_flags(Flags { cy: true, ..Default::default()})
            .with_reg(A, 0b01101110)
            .with_op(Ani(0b11100000))
            .should_have_reg(A, 0b01100000)
            .should_have_carry_equal_to(false);
    }

    #[test]
    fn exclusive_or_register() {
        ProcFixture::new()
            .with_regs(Registers { a: 0b01101110, l: 0b01101001, ..Default::default()})
            .with_op(Xra(L))
            .should_have_reg(A, 0b00000111);
    }

    #[test]
    fn exclusive_or_memory() {
        ProcFixture::new()
            .with_regs(Registers { a: 0b01101110, h: 0x00, l: 0x12, ..Default::default() })
            .with_mem_at_index(0x12, &[0b00101001])
            .with_op(XraM)
            .should_have_reg(A, 0b01000111);
    }

    #[test]
    fn exclusive_or_immediate() {
        ProcFixture::new()
            .with_reg(A, 0b01101110)
            .with_op(Xri(0b00101001))
            .should_have_reg(A, 0b01000111);
    }

    #[test]
    fn inclusive_or_register() {
        ProcFixture::new()
            .with_regs(Registers { a: 0b01101110, l: 0b01101001, ..Default::default()})
            .with_op(Ora(L))
            .should_have_reg(A, 0b01101111);
    }

     #[test]
    fn inclusive_or_memory() {
        ProcFixture::new()
            .with_regs(Registers { a: 0b01101110, h: 0x00, l: 0x12, ..Default::default() })
            .with_mem_at_index(0x12, &[0b00101001])
            .with_op(OraM)
            .should_have_reg(A, 0b01101111);
    }

    #[test]
    fn inclusive_or_immediate() {
        ProcFixture::new()
            .with_reg(A, 0b01101110)
            .with_op(Ori(0b00101001))
            .should_have_reg(A, 0b01101111);
    }

    #[test]
    fn compare_register() {
        let proc8080 = ProcFixture::new()
            .with_reg(A, 0x05)
            .with_reg(B, 0x06)
            .with_op(Cmp(B))
            .unwrap();

        let flags = proc8080.flags();
        assert_eq!(flags.z, false);
        assert_eq!(flags.s, true);
        assert_eq!(flags.p, true);
        assert_eq!(flags.cy, true);
    }

    #[test]
    fn compare_memory() {
        let proc8080 = ProcFixture::new()
            .with_regs(Registers { a: 0x10, h: 0x00, l: 0x12, ..Default::default() })
            .with_mem_at_index(0x12, &[0x02])
            .with_op(CmpM)
            .unwrap();

        let flags = proc8080.flags();
        assert_eq!(flags.z, false);
        assert_eq!(flags.s, false);
        assert_eq!(flags.p, false);
        assert_eq!(flags.cy, false);
    }

    #[test]
    fn compare_immediate() {
        let proc8080 = ProcFixture::new()
            .with_reg(A, 0x10)
            .with_op(Cpi(0x10))
            .unwrap();

        let flags = proc8080.flags();
        assert_eq!(flags.z, true);
        assert_eq!(flags.s, false);
        assert_eq!(flags.p, true);
        assert_eq!(flags.cy, false);
    }

    #[test]
    fn rotate_left_with_carry_false() {
        ProcFixture::new()
            .with_reg(A, 0b10010101)
            .with_op(Rlc)
            .should_have_reg(A, 0b000101011)
            .should_have_carry_equal_to(true);
    }

    #[test]
    fn rotate_right_with_carry_false() {
        ProcFixture::new()
            .with_reg(A, 0b00010101)
            .with_op(Rrc)
            .should_have_reg(A, 0b10001010)
            .should_have_carry_equal_to(true);
    }

    #[test]
    fn rotate_left_through_carry() {
        ProcFixture::new()
            .with_carry(true)
            .with_reg(A, 0x10)
            .with_op(Ral)
            .should_have_reg(A, 0x21)
            .should_have_carry_equal_to(false);
    }

    #[test]
    fn rotate_right_through_carry() {
        ProcFixture::new()
            .with_carry(true)
            .with_reg(A, 0x01)
            .with_op(Rar)
            .should_have_reg(A, 0x80)
            .should_have_carry_equal_to(true);
    }

    #[test]
    fn complement() {
        ProcFixture::new()
            .with_reg(A, 0b01100101)
            .with_op(Cma)
            .should_have_reg(A, 0b10011010);
    }

    #[test]
    fn complement_carry() {
        ProcFixture::new()
            .with_carry(true)
            .with_op(Cmc)
            .should_have_carry_equal_to(false);
    }

    #[test]
    fn set_carry() {
        ProcFixture::new()
            .with_carry(false)
            .with_op(Stc)
            .should_have_carry_equal_to(true);
    }

    #[test]
    fn jump() {
        ProcFixture::new()
            .with_op(Jmp(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn jump_not_zero() {
        ProcFixture::new()
            .with_flags(Flags { z: true, ..Default::default() })
            .with_op(Jnz(0xf0))
            .should_have_pc_equal_to(0)

            .with_flags(Flags { z: false, ..Default::default() })
            .with_op(Jnz(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn jump_zero() {
        ProcFixture::new()
            .with_op(Jz(0xf0))
            .should_have_pc_equal_to(0)

            .with_flags(Flags { z: true, ..Default::default() })
            .with_op(Jz(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn jump_no_carry() {
        ProcFixture::new()
            .with_flags(Flags { cy: true, ..Default::default() })
            .with_op(Jnc(0xf0))
            .should_have_pc_equal_to(0)

            .with_flags(Flags { cy: false, ..Default::default() })
            .with_op(Jnc(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn jump_carry() {
        ProcFixture::new()
            .with_op(Jc(0xf0))
            .should_have_pc_equal_to(0)

            .with_flags(Flags { cy: true, ..Default::default() })
            .with_op(Jc(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn jump_odd() {
        ProcFixture::new()
            .with_flags(Flags { p: true, ..Default::default() })
            .with_op(Jpo(0xf0))
            .should_have_pc_equal_to(0)

            .with_flags(Flags { p: false, ..Default::default() })
            .with_op(Jpo(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn jump_even() {
        ProcFixture::new()
            .with_op(Jpe(0xf0))
            .should_have_pc_equal_to(0)

            .with_flags(Flags { p: true, ..Default::default() })
            .with_op(Jpe(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn jump_positive() {
        ProcFixture::new()
            .with_flags(Flags { s: true, ..Default::default() })
            .with_op(Jp(0xf0))
            .should_have_pc_equal_to(0)

            .with_flags(Default::default())
            .with_op(Jp(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn jump_negative() {
        ProcFixture::new()
            .with_op(Jm(0xf0))
            .should_have_pc_equal_to(0)

            .with_flags(Flags { s: true, ..Default::default() })
            .with_op(Jm(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn call_immediate() {
        ProcFixture::new()
            .with_sp(0xffde)
            .with_pc(0x0311)
            .with_op(Call(0xf201))
            .should_have_sp_equal_to(0xffdc)
            .should_have_pc_equal_to(0xf201)
            .should_have_mem(0xffdd, 0x03)
            .should_have_mem(0xffdc, 0x11);
    }

    #[test]
    fn call_not_zero() {
        ProcFixture::new()
            .with_op(Cnz(0xfef0))
            .should_have_pc_equal_to(0xfef0);
    }

    #[test]
    fn call_zero() {
        ProcFixture::new()
            .with_flags(Flags { z: true, ..Default::default() })
            .with_op(Cz(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn call_no_carry() {
        ProcFixture::new()
            .with_op(Cnc(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn call_carry() {
        ProcFixture::new()
            .with_flags(Flags { cy: true, ..Default::default() })
            .with_op(Cc(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn call_odd() {
        ProcFixture::new()
            .with_op(Cpo(0xf2))
            .should_have_pc_equal_to(0xf2);
    }

    #[test]
    fn call_even() {
        ProcFixture::new()
            .with_flags(Flags { p: true, ..Default::default() })
            .with_op(Cpe(0xf1))
            .should_have_pc_equal_to(0xf1);
    }

    #[test]
    fn call_positive() {
        ProcFixture::new()
            .with_op(Cp(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn call_negative() {
        ProcFixture::new()
            .with_flags(Flags { s: true, ..Default::default() })
            .with_op(Cm(0xf0))
            .should_have_pc_equal_to(0xf0);
    }

    #[test]
    fn return_immediate() {
        ProcFixture::new()
            .with_sp(0xffde)
            .with_pc(0x0311)
            .with_op(Call(0xf201))
            .with_op(Ret)
            .should_have_sp_equal_to(0xffde)
            .should_have_pc_equal_to(0x0311);
    }

    #[test]
    fn ret_not_zero() {
        ProcFixture::new()
            .with_op(Cnz(0xfef0))
            .with_op(Rnz)
            .should_have_pc_equal_to(0x0000)
            .should_have_sp_equal_to(0xffff);
    }

    #[test]
    fn ret_zero() {
        ProcFixture::new()
            .with_flags(Flags { z: true, ..Default::default() })
            .with_op(Cz(0xf0))
            .with_op(Rz)
            .should_have_pc_equal_to(0x0000)
            .should_have_sp_equal_to(0xffff);
    }

    #[test]
    fn ret_zno_carry() {
        ProcFixture::new()
            .with_op(Cnc(0xf0))
            .with_op(Rnc)
            .should_have_pc_equal_to(0x0000)
            .should_have_sp_equal_to(0xffff);
    }

    #[test]
    fn ret_carry() {
        ProcFixture::new()
            .with_flags(Flags { cy: true, ..Default::default() })
            .with_op(Cc(0xf0))
            .with_op(Rc)
            .should_have_pc_equal_to(0x0000)
            .should_have_sp_equal_to(0xffff);
    }

    #[test]
    fn ret_odd() {
        ProcFixture::new()
            .with_op(Cpo(0xf2))
            .with_op(Rpo)
            .should_have_pc_equal_to(0x0000)
            .should_have_sp_equal_to(0xffff);
    }

    #[test]
    fn ret_even() {
        ProcFixture::new()
            .with_flags(Flags { p: true, ..Default::default() })
            .with_op(Cpe(0xf1))
            .with_op(Rpe)
            .should_have_pc_equal_to(0x0000)
            .should_have_sp_equal_to(0xffff);
    }

    #[test]
    fn ret_positive() {
        ProcFixture::new()
            .with_op(Cp(0xf0))
            .with_op(Rp)
            .should_have_pc_equal_to(0x0000)
            .should_have_sp_equal_to(0xffff);
    }

    #[test]
    fn ret_negative() {
        ProcFixture::new()
            .with_flags(Flags { s: true, ..Default::default() })
            .with_op(Cm(0xf0))
            .with_op(Rm)
            .should_have_pc_equal_to(0x0000)
            .should_have_sp_equal_to(0xffff);
    }

    #[test]
    fn reset() {
        ProcFixture::new()
            .with_sp(0xffde)
            .with_pc(0x0311)
            .with_op(Rst(3))
            .should_have_pc_equal_to(24)
            .should_have_sp_equal_to(0xffdc);
    }

    #[test]
    fn jump_hl() {
        ProcFixture::new()
            .with_regs(Registers { h: 0xde, l:0x12, ..Default::default() })
            .with_op(Pchl)
            .should_have_pc_equal_to(0xde12);
    }

    #[test]
    fn push_register_pair() {
        ProcFixture::new()
            .with_regs(Registers { b: 0xde, c:0x12, sp: 0xff08, ..Default::default() })
            .with_op(Push(Reg16::B))
            .should_have_sp_equal_to(0xff06)
            .should_have_mem(0xff06, 0x12)
            .should_have_mem(0xff07, 0xde);
    }

    #[test]
    fn push_processor_status_word() {
        ProcFixture::new()
            .with_reg(A, 0x42)
            .with_sp(0xff08)
            .with_flags(Flags { cy: true, s: true, ac: true, ..Default::default()})
            .with_op(PushPSW)
            .should_have_sp_equal_to(0xff06)
            .should_have_mem(0xff06, 0b10010011)
            .should_have_mem(0xff07, 0x42);
    }

    #[test] 
    fn pop_register() {
        ProcFixture::new()
            .with_regs(Registers { b: 0xde, c:0x12, sp: 0xff08, ..Default::default() })
            .with_op(Push(Reg16::B))
            .with_op(Pop(Reg16::H))
            .should_have_reg16(Reg16::H, 0xde12)
            .should_have_sp_equal_to(0xff08);
    }

    #[test]
    fn pop_processor_status_word() {
        let flags = ProcFixture::new()
            .with_reg(A, 0x42)
            .with_sp(0xff08)
            .with_flags(Flags { cy: true, s: true, ac: true, ..Default::default()})
            .with_op(PushPSW)
            .with_reg(A, 0x00)
            .with_flags(Default::default())
            .with_op(PopPSW)
            .should_have_sp_equal_to(0xff08)
            .should_have_reg(A, 0x42)
            .unwrap().flags;

        assert_eq!(flags.cy, true);
        assert_eq!(flags.s, true);
        assert_eq!(flags.ac, true);
        assert_eq!(flags.z, false);
        assert_eq!(flags.p, false);
    }

    #[test]
    fn xhtl() {
        ProcFixture::new()
            .with_mem_at_index(0xff08, &[0x12, 0x34])
            .with_regs(Registers { h: 0x56, l:0x78, sp: 0xff08, ..Default::default() })
            .with_op(Xthl)
            .should_have_mem(0xff08, 0x78)
            .should_have_mem(0xff09, 0x56)
            .should_have_reg16(Reg16::H, 0x3412);
    }

    #[test]
    fn hl_to_sp() {
        ProcFixture::new()
            .with_regs( Registers { h: 0x12, l: 0x34, ..Default::default()})
            .with_op(Sphl)
            .should_have_sp_equal_to(0x1234);
    }

    #[test]
    fn flag_zero() {
        let proc8080 = ProcFixture::new()
            .with_reg(A, 0xff)
            .with_op(Adi(1))
            .unwrap();

        assert!(proc8080.flags().z)
    }
}