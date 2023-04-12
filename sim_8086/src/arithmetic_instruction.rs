use std::fmt::Display;

use const_panic::concat_panic;

use crate::{effective_address::EffectiveAddress, register::Register};

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum ArithmeticInstructionSelect {
    RegisterToRegister(RegRegArithmetic),
    RegisterToMemory(RegMemArithmetic),
    MemoryToRegister(MemRegArithmetic),
    ImmediateToRegisterByte(Register, u8, bool),
    ImmediateToRegisterWord(Register, u16, bool),
    /// The bool here is "is this actually a u16"
    ImmediateToRegisterOrMemoryByte(EffectiveAddress, u8, bool),
    ImmediateToRegisterOrMemoryWord(EffectiveAddress, u16),
    ImmediateToAccByte(u8),
    ImmediateToAccWord(u16),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct ArithmeticInstruction {
    pub op: ArithmeticOperation,
    pub instruction: ArithmeticInstructionSelect,
}

impl ArithmeticInstruction {
    /// d is expected to be either 0 or 1.
    fn to_byte(s: ArithmeticOperation, d: u8, is_wide: bool) -> u8 {
        // Implicit opcode of 0b000 at the start.
        (s as u8) * 8 + d * 2 + if is_wide { 1 } else { 0 }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::<u8>::with_capacity(2);
        match &self.instruction {
            ArithmeticInstructionSelect::RegisterToRegister(data) => {
                let (source, is_wide_s) = data.source.to_id();
                let (dest, is_wide_d) = data.dest.to_id();
                if is_wide_s != is_wide_d {
                    panic!("Somehow tried to do arithmetic between mismatched sizes")
                }
                let d = 0;
                result.push(Self::to_byte(self.op, d, is_wide_s));

                let mode = 0b11000000u8;
                result.push(mode + source * 8 + dest);
            }
            ArithmeticInstructionSelect::RegisterToMemory(instruction) => {
                let (source, is_wide) = instruction.source.to_id();
                let d = 0; // REG = source
                result.push(Self::to_byte(self.op, d, is_wide));
                instruction.dest.push(source, &mut result);
            }
            ArithmeticInstructionSelect::MemoryToRegister(instruction) => {
                let (dest, is_wide) = instruction.dest.to_id();
                let d = 1; // REG = dest
                result.push(Self::to_byte(self.op, d, is_wide));
                instruction.source.push(dest, &mut result);
            }
            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(dest, data, signed) => {
                let sign_bit = if *signed { 1 } else { 0 };
                let w = sign_bit;
                result.push(0b10000000u8 + 2 * sign_bit + w);
                dest.push(self.op as u8, &mut result);
                result.push(*data);
            }
            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(dest, data) => {
                let sign_bit = 0u8;
                let w = 1u8;
                result.push(0b10000000u8 + 2 * sign_bit + w);
                dest.push(self.op as u8, &mut result);
                result.push((data % 256) as u8);
                result.push((data / 256) as u8)
            }
            ArithmeticInstructionSelect::ImmediateToRegisterByte(reg, data, signed) => {
                let sign_bit = if *signed { 1 } else { 0 };
                let (rm, is_wide) = Register::to_id(reg);
                result.push(0b10000000u8 + 2 * sign_bit + if is_wide { 1 } else { 0 });
                result.push(0b11000000 + (self.op as u8) * 8 + rm);
                result.push(*data);
            }
            ArithmeticInstructionSelect::ImmediateToRegisterWord(reg, data, signed) => {
                let sign_bit = if *signed { 1 } else { 0 };
                let (rm, is_wide) = Register::to_id(reg);
                result.push(0b10000000u8 + 2 * sign_bit + if is_wide { 1 } else { 0 });
                result.push(0b11000000 + (self.op as u8) * 8 + rm);
                result.push((data % 256) as u8);
                if !*signed {
                    result.push((data / 256) as u8);
                }
            }
            ArithmeticInstructionSelect::ImmediateToAccByte(data) => {
                let instruction = 0b00000100 + (self.op as u8) * 8;
                let w = 0u8;
                result.push(instruction + w);
                result.push(*data);
            }
            ArithmeticInstructionSelect::ImmediateToAccWord(data) => {
                let instruction = 0b00000100 + (self.op as u8) * 8;
                let w = 1u8;
                result.push(instruction + w);
                result.push((data % 256) as u8);
                result.push((data / 256) as u8);
            }
        }

        result
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub enum ArithmeticOperation {
    Add = 0,
    Or = 1,
    AddWithCarry = 2,
    SubWithBorrow = 3,
    And = 4,
    Sub = 5,
    Xor = 6,
    Cmp = 7,
}

impl Display for ArithmeticOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ArithmeticOperation::Add => "add",
            ArithmeticOperation::Or => "or",
            ArithmeticOperation::AddWithCarry => "adc",
            ArithmeticOperation::SubWithBorrow => "sbb",
            ArithmeticOperation::And => "and",
            ArithmeticOperation::Sub => "sub",
            ArithmeticOperation::Xor => "xor",
            ArithmeticOperation::Cmp => "cmp",
        })
    }
}

impl ArithmeticOperation {
    pub const fn of_byte(x: u8) -> ArithmeticOperation {
        match x {
            0 => ArithmeticOperation::Add,
            1 => ArithmeticOperation::Or,
            2 => ArithmeticOperation::AddWithCarry,
            3 => ArithmeticOperation::SubWithBorrow,
            4 => ArithmeticOperation::And,
            5 => ArithmeticOperation::Sub,
            6 => ArithmeticOperation::Xor,
            7 => ArithmeticOperation::Cmp,
            _ => concat_panic!("Unrecognised arithmetic op: {}", x),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct RegRegArithmetic {
    pub source: Register,
    pub dest: Register,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct RegMemArithmetic {
    pub source: Register,
    pub dest: EffectiveAddress,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct MemRegArithmetic {
    pub dest: Register,
    pub source: EffectiveAddress,
}