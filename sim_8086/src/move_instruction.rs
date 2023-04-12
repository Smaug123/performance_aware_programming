use std::fmt::Display;

use crate::{register::{Register, RegisterSubset, ByteRegisterSubset}, effective_address::EffectiveAddress};


#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct RegRegMove {
    pub source: Register,
    pub dest: Register,
}

impl RegRegMove {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(2);
        let instruction1 = 0b10001000u8;
        let mut is_wide = 1u8;
        // We always pick the 0 direction, so REG indicates the source.
        let d: u8 = 0;
        // Register-to-register move
        let mode = 0b11000000u8;
        match (&self.dest, &self.source) {
            (
                Register::General(dest, RegisterSubset::Subset(dest_subset)),
                Register::General(source, RegisterSubset::Subset(source_subset)),
            ) => {
                is_wide = 0;
                result.push(instruction1 + 2 * d + is_wide);

                let dest_offset: u8 = 4 * match dest_subset {
                    ByteRegisterSubset::Low => 0,
                    ByteRegisterSubset::High => 1,
                };
                let rm: u8 = dest_offset + dest.to_id();

                let source_offset: u8 = 4 * match source_subset {
                    ByteRegisterSubset::Low => 0,
                    ByteRegisterSubset::High => 1,
                };
                let reg: u8 = source_offset + source.to_id();
                result.push(mode + reg * 8 + rm);
            }
            (
                Register::General(dest, RegisterSubset::All),
                Register::General(source, RegisterSubset::All),
            ) => {
                result.push(instruction1 + 2 * d + is_wide);
                let reg = source.to_id();
                let rm = dest.to_id();
                result.push(mode + reg * 8 + rm);
            }
            (Register::General(dest, RegisterSubset::All), Register::Special(source)) => {
                result.push(instruction1 + 2 * d + is_wide);
                let reg = source.to_id();
                let rm = dest.to_id();
                result.push(mode + reg * 8 + rm);
            }
            (Register::Special(dest), Register::General(source, RegisterSubset::All)) => {
                result.push(instruction1 + 2 * d + is_wide);
                let reg = source.to_id();
                let rm = dest.to_id();
                result.push(mode + reg * 8 + rm);
            }
            (Register::Special(dest), Register::Special(source)) => {
                result.push(instruction1 + 2 * d + is_wide);
                let reg = source.to_id();
                let rm = dest.to_id();
                result.push(mode + reg * 8 + rm);
            }
            (
                Register::General(_, RegisterSubset::Subset(_)),
                Register::General(_, RegisterSubset::All),
            ) => {
                panic!("tried to move wide into narrow")
            }
            (Register::General(_, RegisterSubset::Subset(_)), Register::Special(_)) => {
                panic!("tried to move wide into narrow")
            }
            (Register::Special(_), Register::General(_, RegisterSubset::Subset(_))) => {
                panic!("tried to move narrow into wide")
            }
            (
                Register::General(_, RegisterSubset::All),
                Register::General(_, RegisterSubset::Subset(_)),
            ) => {
                panic!("tried to move narrow into wide")
            }
        }

        result
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct RegMemMove {
    pub source: Register,
    pub dest: EffectiveAddress,
}

impl RegMemMove {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::<u8>::with_capacity(2);

        let instruction1 = 0b10001000u8;
        // Source is the register.
        let d = 0;
        let (source_reg, is_wide) = self.source.to_id();
        result.push(instruction1 + 2 * d + if is_wide { 1 } else { 0 });

        self.dest.push(source_reg, &mut result);

        result
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct MemRegMove {
    pub source: EffectiveAddress,
    pub dest: Register,
}

impl MemRegMove {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(2);

        let instruction1 = 0b10001000u8;
        // Source is the effective address, so REG is the dest.
        let d: u8 = 1;
        let (dest_reg, is_wide) = self.dest.to_id();
        result.push(instruction1 + 2 * d + if is_wide { 1 } else { 0 });

        self.source.push(dest_reg, &mut result);

        result
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum ImmediateToRegister {
    Byte(Register, u8),
    Wide(Register, u16),
}

impl ImmediateToRegister {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::<u8>::with_capacity(2);
        let instruction = 0b10110000u8;
        match self {
            ImmediateToRegister::Byte(register, data) => {
                let (reg, is_wide) = register.to_id();
                if is_wide {
                    panic!("Tried to store a byte into a word register")
                }
                result.push(instruction + reg);
                result.push(*data);
            }
            ImmediateToRegister::Wide(register, data) => {
                result.reserve_exact(1);
                let (reg, is_wide) = register.to_id();
                if !is_wide {
                    panic!("Tried to store a word into a byte register")
                }
                result.push(instruction + 8 + reg);
                result.push((data % 256) as u8);
                result.push((data / 256) as u8);
            }
        }
        result
    }
}

impl Display for ImmediateToRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImmediateToRegister::Byte(dest, value) => {
                f.write_fmt(format_args!("{}, {}", dest, value))
            }
            ImmediateToRegister::Wide(dest, value) => {
                f.write_fmt(format_args!("{}, {}", dest, value))
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum ImmediateToRegisterOrMemory {
    Byte(EffectiveAddress, u8),
    Word(EffectiveAddress, u16),
}

impl ImmediateToRegisterOrMemory {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::<u8>::with_capacity(3);
        let opcode = 0b11000110u8;

        match self {
            ImmediateToRegisterOrMemory::Byte(address, data) => {
                result.push(opcode);
                address.push(0, &mut result);
                result.push(*data);
            }
            ImmediateToRegisterOrMemory::Word(address, data) => {
                result.push(opcode + 1);
                address.push(0, &mut result);
                result.push((data % 256) as u8);
                result.push((data / 256) as u8);
            }
        }

        result
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct MemoryToAccumulator {
    pub address: u16,
    pub is_wide: bool,
}

impl MemoryToAccumulator {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::<u8>::with_capacity(3);
        result.push(0b10100000u8 + if self.is_wide { 1 } else { 0 });
        result.push((self.address % 256) as u8);
        result.push((self.address / 256) as u8);

        result
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub struct AccumulatorToMemory {
    pub address: u16,
    pub is_wide: bool,
}

impl AccumulatorToMemory {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::<u8>::with_capacity(3);
        result.push(0b10100010u8 + if self.is_wide { 1 } else { 0 });
        result.push((self.address % 256) as u8);
        result.push((self.address / 256) as u8);

        result
    }
}


#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum MoveInstruction {
    /// Move a value from one register to another
    RegRegMove(RegRegMove),
    /// Store a value from a register into memory
    RegMemMove(RegMemMove),
    /// Load a value from memory into a register
    MemRegMove(MemRegMove),
    /// Load a literal value into a register
    ImmediateToRegister(ImmediateToRegister),
    /// Load a literal value into a register or into memory
    ImmediateToRegisterOrMemory(ImmediateToRegisterOrMemory),
    /// Load a value from memory into the accumulator
    MemoryToAccumulator(MemoryToAccumulator),
    /// Store a value into memory from the accumulator
    AccumulatorToMemory(AccumulatorToMemory),
}

impl Display for MoveInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MoveInstruction::RegRegMove(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            MoveInstruction::RegMemMove(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            MoveInstruction::MemRegMove(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            MoveInstruction::ImmediateToRegister(instruction) => {
                f.write_fmt(format_args!("mov {}", instruction))
            }
            MoveInstruction::ImmediateToRegisterOrMemory(ImmediateToRegisterOrMemory::Byte(
                address,
                value,
            )) => f.write_fmt(format_args!("mov {}, {}", address, value)),
            MoveInstruction::ImmediateToRegisterOrMemory(ImmediateToRegisterOrMemory::Word(
                address,
                value,
            )) => f.write_fmt(format_args!("mov {}, {}", address, value)),
            MoveInstruction::MemoryToAccumulator(instruction) => f.write_fmt(format_args!(
                "mov a{}, [{}]",
                if instruction.is_wide { 'x' } else { 'l' },
                instruction.address
            )),
            MoveInstruction::AccumulatorToMemory(instruction) => f.write_fmt(format_args!(
                "mov [{}], a{}",
                instruction.address,
                if instruction.is_wide { 'x' } else { 'l' }
            )),
        }
    }
}

impl MoveInstruction {
    pub(crate) fn to_bytes(&self) -> Vec<u8> {
        match self {
            MoveInstruction::RegMemMove(mov) => mov.to_bytes(),
            MoveInstruction::MemRegMove(mov) => mov.to_bytes(),
            MoveInstruction::ImmediateToRegister(mov) => mov.to_bytes(),
            MoveInstruction::ImmediateToRegisterOrMemory(mov) => mov.to_bytes(),
            MoveInstruction::MemoryToAccumulator(mov) => mov.to_bytes(),
            MoveInstruction::AccumulatorToMemory(mov) => mov.to_bytes(),
            MoveInstruction::RegRegMove(mov) => mov.to_bytes(),
        }
    }
}

#[cfg(test)]
mod test_move_instruction {
    use crate::{move_instruction::{MoveInstruction, MemRegMove}, register::{RegisterSubset, GeneralRegister, Register}, instruction::Instruction, effective_address::EffectiveAddress};

    #[test]
    fn mem_reg_move_to_bytes() {
        let i = MoveInstruction::MemRegMove(MemRegMove {
            source: EffectiveAddress::BasePointer(0),
            dest: Register::General(GeneralRegister::D, RegisterSubset::All),
        });
        assert_eq!(Instruction::<i8>::Move(i).to_bytes(), vec![139, 86, 0]);
    }
}