use std::fmt::Display;

use arbitrary::Arbitrary;

use crate::{
    effective_address::EffectiveAddress,
    register::{ByteRegisterSubset, Register, RegisterSubset, SegmentRegister},
};

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
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

    const fn length(&self) -> u8 {
        2
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct RegToMemMove {
    pub source: Register,
    pub dest: EffectiveAddress,
}

impl RegToMemMove {
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
    fn length(&self) -> u8 {
        1 + self.dest.length()
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct MemToRegMove {
    pub source: EffectiveAddress,
    pub dest: Register,
}

impl MemToRegMove {
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

    fn length(&self) -> u8 {
        1 + self.source.length()
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
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
    fn length(&self) -> u8 {
        match self {
            ImmediateToRegister::Byte(_, _) => 2,
            ImmediateToRegister::Wide(_, _) => 3,
        }
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

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum ImmediateToMemory {
    Byte(EffectiveAddress, u8),
    Word(EffectiveAddress, u16),
}

impl ImmediateToMemory {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::<u8>::with_capacity(3);
        let opcode = 0b11000110u8;

        match self {
            ImmediateToMemory::Byte(address, data) => {
                result.push(opcode);
                address.push(0, &mut result);
                result.push(*data);
            }
            ImmediateToMemory::Word(address, data) => {
                result.push(opcode + 1);
                address.push(0, &mut result);
                result.push((data % 256) as u8);
                result.push((data / 256) as u8);
            }
        }

        result
    }
    fn length(&self) -> u8 {
        match self {
            ImmediateToMemory::Byte(address, _) => 2 + address.length(),
            ImmediateToMemory::Word(address, _) => 3 + address.length(),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
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

    fn length(&self) -> u8 {
        3
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
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
    fn length(&self) -> u8 {
        3
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct SegmentToMemory {
    pub source: SegmentRegister,
    pub dest: EffectiveAddress,
}

impl SegmentToMemory {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(4);
        result.push(0b10001100);

        self.dest.push(self.source as u8, &mut result);

        result
    }
    fn length(&self) -> u8 {
        1 + self.dest.length()
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct MemoryToSegment {
    pub dest: SegmentRegister,
    pub source: EffectiveAddress,
}

impl MemoryToSegment {
    fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(4);
        result.push(0b10001110);

        self.source.push(self.dest as u8, &mut result);

        result
    }

    const fn length(&self) -> u8 {
        1 + self.source.length()
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct SegmentToRegister {
    pub source: SegmentRegister,
    pub dest: Register,
}

impl SegmentToRegister {
    fn to_bytes(&self) -> Vec<u8> {
        let mode = 0b11u8;
        let sr = self.source as u8;
        let (rm, is_wide) = self.dest.to_id();
        if !is_wide {
            panic!("Tried to move a segment register to a byte register");
        }
        vec![0b10001100, mode * 64 + sr * 8 + rm]
    }

    const fn length(&self) -> u8 {
        2
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct RegisterToSegment {
    pub dest: SegmentRegister,
    pub source: Register,
}

impl RegisterToSegment {
    fn to_bytes(&self) -> Vec<u8> {
        let mode = 0b11u8;
        let sr = self.dest as u8;
        let (rm, is_wide) = self.source.to_id();
        if !is_wide {
            panic!("Tried to move a byte register to a segment register");
        }
        vec![0b10001110, mode * 64 + sr * 8 + rm]
    }

    const fn length(&self) -> u8 {
        2
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum MoveInstruction {
    /// Move a value from one register to another
    RegRegMove(RegRegMove),
    /// Store a value from a register into memory
    RegMemMove(RegToMemMove),
    /// Load a value from memory into a register
    MemRegMove(MemToRegMove),
    /// Load a literal value into a register
    ImmediateToRegister(ImmediateToRegister),
    /// Load a literal value into a register or into memory
    ImmediateToMemory(ImmediateToMemory),
    /// Load a value from memory into the accumulator
    MemoryToAccumulator(MemoryToAccumulator),
    /// Store a value into memory from the accumulator
    AccumulatorToMemory(AccumulatorToMemory),
    SegmentToMemory(SegmentToMemory),
    MemoryToSegment(MemoryToSegment),
    SegmentToRegister(SegmentToRegister),
    RegisterToSegment(RegisterToSegment),
}

impl Display for MoveInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MoveInstruction::RegRegMove(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            MoveInstruction::RegMemMove(mov) => f.write_fmt(format_args!(
                "mov {}{}, {}",
                if mov.source.is_wide() {
                    "word "
                } else {
                    "byte "
                },
                mov.dest,
                mov.source
            )),
            MoveInstruction::MemRegMove(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            MoveInstruction::ImmediateToRegister(instruction) => {
                f.write_fmt(format_args!("mov {}", instruction))
            }
            MoveInstruction::ImmediateToMemory(ImmediateToMemory::Byte(address, value)) => {
                f.write_fmt(format_args!("mov byte {}, {}", address, value))
            }
            MoveInstruction::ImmediateToMemory(ImmediateToMemory::Word(address, value)) => {
                f.write_fmt(format_args!("mov word {}, {}", address, value))
            }
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
            MoveInstruction::SegmentToMemory(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            MoveInstruction::SegmentToRegister(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            MoveInstruction::RegisterToSegment(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            MoveInstruction::MemoryToSegment(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
        }
    }
}

impl MoveInstruction {
    pub(crate) fn to_bytes(&self) -> Vec<u8> {
        match self {
            MoveInstruction::RegMemMove(mov) => mov.to_bytes(),
            MoveInstruction::MemRegMove(mov) => mov.to_bytes(),
            MoveInstruction::ImmediateToRegister(mov) => mov.to_bytes(),
            MoveInstruction::ImmediateToMemory(mov) => mov.to_bytes(),
            MoveInstruction::MemoryToAccumulator(mov) => mov.to_bytes(),
            MoveInstruction::AccumulatorToMemory(mov) => mov.to_bytes(),
            MoveInstruction::RegRegMove(mov) => mov.to_bytes(),
            MoveInstruction::MemoryToSegment(mov) => mov.to_bytes(),
            MoveInstruction::SegmentToMemory(mov) => mov.to_bytes(),
            MoveInstruction::SegmentToRegister(mov) => mov.to_bytes(),
            MoveInstruction::RegisterToSegment(mov) => mov.to_bytes(),
        }
    }

    pub(crate) fn length(&self) -> u8 {
        match self {
            MoveInstruction::RegMemMove(mov) => mov.length(),
            MoveInstruction::MemRegMove(mov) => mov.length(),
            MoveInstruction::ImmediateToRegister(mov) => mov.length(),
            MoveInstruction::ImmediateToMemory(mov) => mov.length(),
            MoveInstruction::MemoryToAccumulator(mov) => mov.length(),
            MoveInstruction::AccumulatorToMemory(mov) => mov.length(),
            MoveInstruction::RegRegMove(mov) => mov.length(),
            MoveInstruction::MemoryToSegment(mov) => mov.length(),
            MoveInstruction::SegmentToMemory(mov) => mov.length(),
            MoveInstruction::SegmentToRegister(mov) => mov.length(),
            MoveInstruction::RegisterToSegment(mov) => mov.length(),
        }
    }

    #[must_use]
    pub fn clock_count(&self) -> (u32, String) {
        match self {
            MoveInstruction::RegRegMove(_) => (2, "".to_owned()),
            MoveInstruction::RegMemMove(instr) => {
                let (count, result) = instr.dest.clock_count();
                (count + 9, format!("9 {result}"))
            }
            MoveInstruction::MemRegMove(instr) => {
                let (count, result) = instr.source.clock_count();
                (count + 8, format!("8 {result}"))
            }
            MoveInstruction::ImmediateToRegister(_) => (4, "".to_owned()),
            MoveInstruction::ImmediateToMemory(instr) => {
                let dest = match instr {
                    ImmediateToMemory::Byte(addr, _) => addr,
                    ImmediateToMemory::Word(addr, _) => addr,
                };
                let (count, result) = dest.clock_count();
                (10 + count, format!("10 {result}"))
            }
            MoveInstruction::MemoryToAccumulator(_) => (10, "".to_owned()),
            MoveInstruction::AccumulatorToMemory(_) => (10, "".to_owned()),
            MoveInstruction::SegmentToMemory(instr) => {
                let (count, result) = instr.dest.clock_count();
                (9 + count, format!("9 {result}"))
            }
            MoveInstruction::MemoryToSegment(instr) => {
                let (count, result) = instr.source.clock_count();
                (9 + count, format!("9, {result}"))
            }
            MoveInstruction::SegmentToRegister(_) => (2, "".to_owned()),
            MoveInstruction::RegisterToSegment(_) => (2, "".to_owned()),
        }
    }
}

#[cfg(test)]
mod test_move_instruction {
    use crate::{
        effective_address::EffectiveAddress,
        instruction::Instruction,
        move_instruction::{MemToRegMove, MoveInstruction},
        register::{GeneralRegister, Register, RegisterSubset},
    };

    #[test]
    fn mem_reg_move_to_bytes() {
        let i = MoveInstruction::MemRegMove(MemToRegMove {
            source: EffectiveAddress::BasePointer(0),
            dest: Register::General(GeneralRegister::D, RegisterSubset::All),
        });
        assert_eq!(Instruction::<i8>::Move(i).to_bytes(), vec![139, 86, 0]);
    }
}
