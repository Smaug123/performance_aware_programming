mod assembly;
mod register;

use std::{
    fmt::{Display, Write},
    fs,
    path::Path,
};

use clap::Parser;
use const_panic::concat_panic;
use register::{ByteRegisterSubset, Register, RegisterSubset};

#[derive(Eq, PartialEq, Debug)]
pub struct RegRegMove {
    source: Register,
    dest: Register,
}

#[derive(Eq, PartialEq, Debug)]
pub enum SourceDest {
    Source,
    Dest,
}

impl Display for SourceDest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceDest::Source => f.write_char('s'),
            SourceDest::Dest => f.write_char('d'),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum Base {
    Bx,
    Bp,
}

impl Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Base::Bx => f.write_str("bx"),
            Base::Bp => f.write_str("bp"),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum WithOffset<T> {
    Basic(T),
    WithU8(T, u8),
    WithU16(T, u16),
}

impl<T> Display for WithOffset<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WithOffset::Basic(t) => f.write_fmt(format_args!("{}", t)),
            WithOffset::WithU8(t, offset) => f.write_fmt(format_args!("[{} + {}]", t, offset)),
            WithOffset::WithU16(t, offset) => f.write_fmt(format_args!("[{} + {}]", t, offset)),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum EffectiveAddress {
    Sum(WithOffset<(Base, SourceDest)>),
    SpecifiedIn(WithOffset<SourceDest>),
    Bx(WithOffset<()>),
    Direct(u16),
    BasePointer(u8),
    BasePointerWide(u16),
}

impl Display for EffectiveAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EffectiveAddress::Sum(w) => match w {
                WithOffset::Basic((base, source_dest)) => {
                    f.write_fmt(format_args!("[{} + {}i]", base, source_dest))
                }
                WithOffset::WithU8((base, source_dest), offset) => {
                    f.write_fmt(format_args!("[{} + {}i + {}]", base, source_dest, offset))
                }
                WithOffset::WithU16((base, source_dest), offset) => {
                    f.write_fmt(format_args!("[{} + {}i + {}]", base, source_dest, offset))
                }
            },
            EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)) => {
                f.write_fmt(format_args!("[{}i]", source_dest))
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, offset)) => {
                f.write_fmt(format_args!("[{}i + {}]", source_dest, offset))
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU16(source_dest, offset)) => {
                f.write_fmt(format_args!("[{}i + {}]", source_dest, offset))
            }
            EffectiveAddress::Bx(offset) => match offset {
                WithOffset::Basic(()) => f.write_str("bx"),
                WithOffset::WithU8((), offset) => f.write_fmt(format_args!("[bx + {}]", offset)),
                WithOffset::WithU16((), offset) => f.write_fmt(format_args!("[bx + {}]", offset)),
            },
            EffectiveAddress::Direct(location) => f.write_fmt(format_args!("[{}]", location)),
            EffectiveAddress::BasePointer(offset) => f.write_fmt(format_args!("[bp + {}]", offset)),
            EffectiveAddress::BasePointerWide(offset) => {
                f.write_fmt(format_args!("[bp + {}]", offset))
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct RegMemMove {
    source: Register,
    dest: EffectiveAddress,
}

#[derive(Eq, PartialEq, Debug)]
pub struct MemRegMove {
    source: EffectiveAddress,
    dest: Register,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ImmediateToRegister {
    Byte(Register, u8),
    Wide(Register, u16),
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

#[derive(Eq, PartialEq, Debug)]
pub enum ImmediateToRegisterOrMemory {
    Byte(EffectiveAddress, u8),
    Word(EffectiveAddress, u16),
}

#[derive(Eq, PartialEq, Debug)]
pub struct MemoryToAccumulator {
    address: u16,
    is_wide: bool,
}

#[derive(Eq, PartialEq, Debug)]
pub struct AccumulatorToMemory {
    address: u16,
    is_wide: bool,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Jump {
    Je,
    Jl,
    Jle,
    Jb,
    Jbe,
    Jp,
    Jo,
    Js,
    Jne,
    Jnl,
    Jnle,
    Jnb,
    Jnbe,
    Jnp,
    Jno,
    Jns,
    Loop,
    Loopz,
    Loopnz,
    Jcxz,
}

impl Display for Jump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Jump::Je => "je",
            Jump::Jl => "jl",
            Jump::Jle => "jle",
            Jump::Jb => "jb",
            Jump::Jbe => "jbe",
            Jump::Jp => "jp",
            Jump::Jo => "jo",
            Jump::Js => "js",
            Jump::Jne => "jne",
            Jump::Jnl => "jnl",
            Jump::Jnle => "jnle",
            Jump::Jnb => "jnb",
            Jump::Jnbe => "jnbe",
            Jump::Jnp => "jnp",
            Jump::Jno => "jno",
            Jump::Jns => "jns",
            Jump::Loop => "loop",
            Jump::Loopz => "loopz",
            Jump::Loopnz => "loopnz",
            Jump::Jcxz => "jcxz",
        })
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
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

#[derive(Eq, PartialEq, Debug)]
pub struct RegRegArithmetic {
    source: Register,
    dest: Register,
}

#[derive(Eq, PartialEq, Debug)]
pub struct RegMemArithmetic {
    source: Register,
    dest: EffectiveAddress,
}

#[derive(Eq, PartialEq, Debug)]
pub struct MemRegArithmetic {
    dest: Register,
    source: EffectiveAddress,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ArithmeticInstructionSelect {
    RegisterToRegister(RegRegArithmetic),
    RegisterToMemory(RegMemArithmetic),
    MemoryToRegister(MemRegArithmetic),
    ImmediateToRegisterByte(Register, u8, bool),
    ImmediateToRegisterWord(Register, u16, bool),
    ImmediateToRegisterOrMemoryByte(EffectiveAddress, u8, bool),
    ImmediateToRegisterOrMemoryWord(EffectiveAddress, u16, bool),
    ImmediateToAccByte(u8),
    ImmediateToAccWord(u16),
}

#[derive(Eq, PartialEq, Debug)]
pub struct ArithmeticInstruction {
    op: ArithmeticOperation,
    instruction: ArithmeticInstructionSelect,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Instruction {
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
    /// Perform arithmetic
    Arithmetic(ArithmeticInstruction),
    Jump(Jump, i8),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::RegRegMove(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            Instruction::RegMemMove(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            Instruction::MemRegMove(mov) => {
                f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source))
            }
            Instruction::ImmediateToRegister(instruction) => {
                f.write_fmt(format_args!("mov {}", instruction))
            }
            Instruction::ImmediateToRegisterOrMemory(ImmediateToRegisterOrMemory::Byte(
                address,
                value,
            )) => f.write_fmt(format_args!("mov {}, {}", address, value)),
            Instruction::ImmediateToRegisterOrMemory(ImmediateToRegisterOrMemory::Word(
                address,
                value,
            )) => f.write_fmt(format_args!("mov {}, {}", address, value)),
            Instruction::MemoryToAccumulator(instruction) => f.write_fmt(format_args!(
                "mov a{}, [{}]",
                if instruction.is_wide { 'x' } else { 'l' },
                instruction.address
            )),
            Instruction::AccumulatorToMemory(instruction) => f.write_fmt(format_args!(
                "mov [{}], a{}",
                instruction.address,
                if instruction.is_wide { 'x' } else { 'l' }
            )),
            Instruction::Arithmetic(op) => {
                f.write_fmt(format_args!("{} ", op.op))?;
                match &op.instruction {
                    ArithmeticInstructionSelect::RegisterToRegister(inst) => {
                        f.write_fmt(format_args!("{}, {}", inst.dest, inst.source))
                    }
                    ArithmeticInstructionSelect::RegisterToMemory(inst) => {
                        f.write_fmt(format_args!("{}, {}", inst.dest, inst.source))
                    }
                    ArithmeticInstructionSelect::MemoryToRegister(inst) => {
                        f.write_fmt(format_args!("{}, {}", inst.dest, inst.source))
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(addr, data, _) => {
                        f.write_fmt(format_args!("{}, {}", addr, data))
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(addr, data, _) => {
                        f.write_fmt(format_args!("{}, {}", addr, data))
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterByte(addr, data, signed) => {
                        if *signed {
                            f.write_fmt(format_args!("{}, {} ; signed", addr, *data))
                        } else {
                            f.write_fmt(format_args!("{}, {}", addr, data))
                        }
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterWord(addr, data, signed) => {
                        if *signed {
                            f.write_fmt(format_args!("{}, {} ; signed", addr, *data))
                        } else {
                            f.write_fmt(format_args!("{}, {}", addr, data))
                        }
                    }
                    ArithmeticInstructionSelect::ImmediateToAccByte(data) => {
                        f.write_fmt(format_args!("al, {}", data))
                    }
                    ArithmeticInstructionSelect::ImmediateToAccWord(data) => {
                        f.write_fmt(format_args!("ax, {}", data))
                    }
                }
            }
            Instruction::Jump(instruction, offset) => {
                f.write_fmt(format_args!("{} ; {}", instruction, offset))
            }
        }
    }
}

impl Instruction {
    fn push_effective_address(address: &EffectiveAddress, reg: u8, result: &mut Vec<u8>) {
        match address {
            EffectiveAddress::Sum(WithOffset::Basic((base, source_dest))) => {
                let mode = 0u8;
                let rm = match base {
                    Base::Bx => 0u8,
                    Base::Bp => 2,
                } + match source_dest {
                    SourceDest::Source => 0,
                    SourceDest::Dest => 1,
                };
                result.push(mode * 64 + reg * 8 + rm);
            }
            EffectiveAddress::Sum(WithOffset::WithU8((base, source_dest), offset)) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = match base {
                    Base::Bx => 0u8,
                    Base::Bp => 2,
                } + match source_dest {
                    SourceDest::Source => 0,
                    SourceDest::Dest => 1,
                };
                result.push(mode * 64 + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::Sum(WithOffset::WithU16((base, source_dest), offset)) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = match base {
                    Base::Bx => 0u8,
                    Base::Bp => 2,
                } + match source_dest {
                    SourceDest::Source => 0,
                    SourceDest::Dest => 1,
                };
                result.push(mode * 64 + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)) => {
                let mode = 0u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode * 64 + reg * 8 + rm);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, offset)) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode * 64 + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU16(source_dest, offset)) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode * 64 + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::Bx(WithOffset::Basic(())) => {
                let mode = 0u8;
                let rm = 7u8;
                result.push(mode * 64 + reg * 8 + rm);
            }
            EffectiveAddress::Bx(WithOffset::WithU8((), offset)) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = 7u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::Bx(WithOffset::WithU16((), offset)) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = 7u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::Direct(address) => {
                result.reserve_exact(2);
                let mode = 0u8;
                let rm = 6u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push((address % 256) as u8);
                result.push((address / 256) as u8);
            }
            EffectiveAddress::BasePointer(offset) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = 6u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::BasePointerWide(offset) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = 6u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
        }
    }

    fn to_bytes(&self) -> Vec<u8> {
        match self {
            Instruction::RegRegMove(mov) => {
                let mut result = Vec::with_capacity(2);
                let instruction1 = 0b10001000u8;
                let mut is_wide = 1u8;
                // We always pick the 0 direction, so REG indicates the source.
                let d: u8 = 0;
                // Register-to-register move
                let mode = 0b11000000u8;
                match (&mov.dest, &mov.source) {
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

            Instruction::RegMemMove(mov) => {
                let mut result = Vec::<u8>::with_capacity(2);

                let instruction1 = 0b10001000u8;
                // Source is the register.
                let d = 0;
                let (source_reg, is_wide) = mov.source.to_id();
                result.push(instruction1 + 2 * d + if is_wide { 1 } else { 0 });

                Self::push_effective_address(&mov.dest, source_reg, &mut result);

                result
            }
            Instruction::MemRegMove(mov) => {
                let mut result = Vec::with_capacity(2);

                let instruction1 = 0b10001000u8;
                // Source is the effective address, so REG is the dest.
                let d: u8 = 1;
                let (dest_reg, is_wide) = mov.dest.to_id();
                result.push(instruction1 + 2 * d + if is_wide { 1 } else { 0 });

                Self::push_effective_address(&mov.source, dest_reg, &mut result);

                result
            }

            Instruction::ImmediateToRegister(mov) => {
                let mut result = Vec::<u8>::with_capacity(2);
                let instruction = 0b10110000u8;
                match mov {
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

            Instruction::ImmediateToRegisterOrMemory(mov) => {
                let mut result = Vec::<u8>::with_capacity(3);
                let opcode = 0b11000110u8;

                match mov {
                    ImmediateToRegisterOrMemory::Byte(address, data) => {
                        result.push(opcode);
                        Self::push_effective_address(address, 0, &mut result);
                        result.push(*data);
                    }
                    ImmediateToRegisterOrMemory::Word(address, data) => {
                        result.push(opcode + 1);
                        Self::push_effective_address(address, 0, &mut result);
                        result.push((data % 256) as u8);
                        result.push((data / 256) as u8);
                    }
                }

                result
            }

            Instruction::MemoryToAccumulator(mov) => {
                let mut result = Vec::<u8>::with_capacity(3);
                result.push(0b10100000u8 + if mov.is_wide { 1 } else { 0 });
                result.push((mov.address % 256) as u8);
                result.push((mov.address / 256) as u8);

                result
            }

            Instruction::AccumulatorToMemory(mov) => {
                let mut result = Vec::<u8>::with_capacity(3);
                result.push(0b10100010u8 + if mov.is_wide { 1 } else { 0 });
                result.push((mov.address % 256) as u8);
                result.push((mov.address / 256) as u8);

                result
            }

            Instruction::Arithmetic(instruction) => {
                let mut result = Vec::<u8>::with_capacity(2);
                match &instruction.instruction {
                    ArithmeticInstructionSelect::RegisterToRegister(data) => {
                        todo!();
                        let (rm, is_wide) = data.dest.to_id();
                        let d = 0;
                        result.push(
                            0b00000000u8
                                + (instruction.op as u8) * 8
                                + d * 2
                                + if is_wide { 1 } else { 0 },
                        );

                        let mode = 0b11000000u8;
                        match (&data.source, &data.dest) {
                            (
                                Register::General(source, RegisterSubset::Subset(source_subset)),
                                Register::General(dest, RegisterSubset::Subset(dest_subset)),
                            ) => {
                                let dest_offset: u8 = 4 * match dest_subset {
                                    ByteRegisterSubset::Low => 0,
                                    ByteRegisterSubset::High => 1,
                                };
                                let source_offset: u8 = 4 * match source_subset {
                                    ByteRegisterSubset::Low => 0,
                                    ByteRegisterSubset::High => 1,
                                };
                                let reg: u8 = source_offset + source.to_id();
                                result.push(mode + reg * 8 + rm);
                            }
                            (
                                Register::General(source, RegisterSubset::All),
                                Register::General(dest, RegisterSubset::All),
                            ) => {
                                let reg = source.to_id();
                                result.push(mode + reg * 8 + rm);
                            }
                            (Register::General(_, _), Register::General(_, _)) => {
                                panic!("Tried to add mismatched register sizes");
                            }
                            (Register::General(_, _), Register::Special(_)) => todo!(),
                            (Register::Special(_), Register::General(_, _)) => todo!(),
                            (Register::Special(_), Register::Special(_)) => todo!(),
                        }
                    }
                    ArithmeticInstructionSelect::RegisterToMemory(_) => todo!(),
                    ArithmeticInstructionSelect::MemoryToRegister(_) => todo!(),
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(
                        dest,
                        data,
                        signed,
                    ) => {
                        let sign_bit = if *signed { 1 } else { 0 };
                        let w = 0u8;
                        result.push(0b10000000u8 + 2 * sign_bit + w);
                        Self::push_effective_address(&dest, instruction.op as u8, &mut result);
                        result.push(*data);
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(
                        dest,
                        data,
                        signed,
                    ) => {
                        let sign_bit = if *signed { 1 } else { 0 };
                        let w = 1u8;
                        result.push(0b10000000u8 + 2 * sign_bit + w);
                        Self::push_effective_address(&dest, instruction.op as u8, &mut result);
                        result.push((data % 256) as u8);
                        result.push((data / 256) as u8);
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterByte(reg, data, signed) => {
                        let sign_bit = if *signed { 1 } else { 0 };
                        let (rm, is_wide) = Register::to_id(reg);
                        result.push(0b10000000u8 + 2 * sign_bit + if is_wide { 1 } else { 0 });
                        result.push(0b11000000 + (instruction.op as u8) * 8 + rm);
                        result.push(*data);
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterWord(reg, data, signed) => {
                        let sign_bit = if *signed { 1 } else { 0 };
                        let (rm, is_wide) = Register::to_id(reg);
                        result.push(0b10000000u8 + 2 * sign_bit + if is_wide { 1 } else { 0 });
                        result.push(0b11000000 + (instruction.op as u8) * 8 + rm);
                        result.push((data % 256) as u8);
                        result.push((data / 256) as u8);
                    }
                    ArithmeticInstructionSelect::ImmediateToAccByte(data) => {
                        let instruction = 0b00000100 + (instruction.op as u8) * 8;
                        let w = 0u8;
                        result.push(instruction + w);
                        result.push(*data);
                    }
                    ArithmeticInstructionSelect::ImmediateToAccWord(data) => {
                        let instruction = 0b00000100 + (instruction.op as u8) * 8;
                        let w = 1u8;
                        result.push(instruction + w);
                        result.push((data % 256) as u8);
                        result.push((data / 256) as u8);
                    }
                }

                result
            }

            Instruction::Jump(instruction, offset) => {
                let mut result = Vec::<u8>::with_capacity(2);

                result.push(match instruction {
                    Jump::Je => 0b01110100,
                    Jump::Jl => 0b11111100,
                    Jump::Jle => 0b01111110,
                    Jump::Jb => 0b01110010,
                    Jump::Jbe => 0b01110110,
                    Jump::Jp => 0b01111010,
                    Jump::Jo => 0b01110000,
                    Jump::Js => 0b01111000,
                    Jump::Jne => 0b01110101,
                    Jump::Jnl => 0b01111101,
                    Jump::Jnle => 0b01111111,
                    Jump::Jnb => 0b01110011,
                    Jump::Jnbe => 0b01110111,
                    Jump::Jnp => 0b01111011,
                    Jump::Jno => 0b01110001,
                    Jump::Jns => 0b01111001,
                    Jump::Loop => 0b11100010,
                    Jump::Loopz => 0b11100001,
                    Jump::Loopnz => 0b11100000,
                    Jump::Jcxz => 0b11100011,
                });

                result.push(if *offset >= 0 {
                    *offset as u8
                } else {
                    255 - (-*offset) as u8 + 1
                });
                result
            }
        }
    }

    fn mode_rm_to_eaddr<I>(mode: u8, rm: u8, bytes: &mut I) -> EffectiveAddress
    where
        I: Iterator<Item = u8>,
    {
        let source_dest = if rm % 2 == 0 {
            SourceDest::Source
        } else {
            SourceDest::Dest
        };
        let base = if (rm / 2) % 2 == 0 {
            Base::Bx
        } else {
            Base::Bp
        };
        let displacement_low = if rm == 6 || mode > 0 {
            bytes.next().expect("required an 8-bit displacement")
        } else {
            0
        };
        let displacement_high = if (rm == 6 && mode == 0) || mode == 2 {
            let high = bytes.next().expect("required a 16-bit displacement");
            (high as u16) * 256 + (displacement_low as u16)
        } else {
            0
        };

        if rm < 4 {
            match mode {
                0 => EffectiveAddress::Sum(WithOffset::Basic((base, source_dest))),
                1 => {
                    EffectiveAddress::Sum(WithOffset::WithU8((base, source_dest), displacement_low))
                }
                2 => EffectiveAddress::Sum(WithOffset::WithU16(
                    (base, source_dest),
                    displacement_high,
                )),
                _ => panic!("Got bad mode: {}", mode),
            }
        } else if rm < 6 {
            match mode {
                0 => EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)),
                1 => {
                    EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, displacement_low))
                }
                2 => EffectiveAddress::SpecifiedIn(WithOffset::WithU16(
                    source_dest,
                    displacement_high,
                )),
                _ => panic!("Got bad mode: {}", mode),
            }
        } else if rm == 6 {
            match mode {
                0 => EffectiveAddress::Direct(displacement_high),
                1 => EffectiveAddress::BasePointer(displacement_low),
                2 => EffectiveAddress::BasePointerWide(displacement_high),
                _ => panic!("Got bad mode: {}", mode),
            }
        } else {
            assert_eq!(rm, 7);
            match mode {
                0 => EffectiveAddress::Bx(WithOffset::Basic(())),
                1 => EffectiveAddress::Bx(WithOffset::WithU8((), displacement_low)),
                2 => EffectiveAddress::Bx(WithOffset::WithU16((), displacement_high)),
                _ => panic!("Got bad mode: {}", mode),
            }
        }
    }

    fn consume<I>(bytes: &mut I) -> Option<Instruction>
    where
        I: Iterator<Item = u8>,
    {
        if let Some(b) = bytes.next() {
            if (b & 0b11111100u8) == 0b10001000u8 {
                let d = (b / 2) % 2;
                let is_wide = b % 2 == 1;
                if let Some(mod_reg_rm) = bytes.next() {
                    let mode = (mod_reg_rm & 0b11000000) / 64;
                    let reg = (mod_reg_rm & 0b00111000) / 8;
                    let rm = mod_reg_rm & 0b00000111;
                    let reg = Register::of_id(reg, is_wide);

                    if mode == 3 {
                        let rm = Register::of_id(rm, is_wide);

                        let instruction = if d == 0 {
                            RegRegMove {
                                source: reg,
                                dest: rm,
                            }
                        } else {
                            RegRegMove {
                                source: rm,
                                dest: reg,
                            }
                        };
                        Some(Instruction::RegRegMove(instruction))
                    } else {
                        let mem_location = Self::mode_rm_to_eaddr(mode, rm, bytes);
                        if d == 0 {
                            Some(Instruction::RegMemMove(RegMemMove {
                                source: reg,
                                dest: mem_location,
                            }))
                        } else {
                            Some(Instruction::MemRegMove(MemRegMove {
                                dest: reg,
                                source: mem_location,
                            }))
                        }
                    }
                } else {
                    panic!("mov required a second byte")
                }
            } else if (b & 0b11110000u8) == 0b10110000u8 {
                // Immediate to register
                let w = (b / 8) % 2;

                if w == 1 {
                    let reg = Register::of_id(b % 8, true);
                    let next_low = bytes.next().unwrap() as u16;
                    let next_high = bytes.next().unwrap() as u16;
                    Some(Instruction::ImmediateToRegister(ImmediateToRegister::Wide(
                        reg,
                        next_low + 256 * next_high,
                    )))
                } else {
                    let reg = Register::of_id(b % 8, false);
                    let next_low = bytes.next().unwrap();
                    Some(Instruction::ImmediateToRegister(ImmediateToRegister::Byte(
                        reg, next_low,
                    )))
                }
            } else if (b & 0b11111110) == 0b10100000 {
                // Memory to accumulator
                let w = b % 2;
                let addr_low = bytes.next().unwrap() as u16;
                let addr_high = bytes.next().unwrap() as u16 * 256;
                Some(Instruction::MemoryToAccumulator(MemoryToAccumulator {
                    address: addr_high + addr_low,
                    is_wide: w == 1,
                }))
            } else if (b & 0b11111110) == 0b10100010 {
                // Accumulator to memory
                let w = b % 2;
                let addr_low = bytes.next().unwrap() as u16;
                let addr_high = bytes.next().unwrap() as u16 * 256;
                Some(Instruction::AccumulatorToMemory(AccumulatorToMemory {
                    address: addr_high + addr_low,
                    is_wide: w == 1,
                }))
            } else if (b & 0b11111110) == 0b11000110 {
                // Immediate to register/memory
                let w = b % 2;
                let mod_reg_rm = bytes.next().unwrap();
                let mode = (mod_reg_rm & 0b11000000) / 64;
                let reg = (mod_reg_rm & 0b00111000) / 8;
                let rm = mod_reg_rm & 0b00000111;
                assert_eq!(reg, 0);
                let dest = Self::mode_rm_to_eaddr(mode, rm, bytes);

                let data_low = bytes.next().unwrap();
                if w == 1 {
                    let data_high = bytes.next().unwrap() as u16 * 256;
                    Some(Instruction::ImmediateToRegisterOrMemory(
                        ImmediateToRegisterOrMemory::Word(dest, data_high + data_low as u16),
                    ))
                } else {
                    Some(Instruction::ImmediateToRegisterOrMemory(
                        ImmediateToRegisterOrMemory::Byte(dest, data_low),
                    ))
                }
            } else if (b & 0b11000100) == 0b00000000u8 {
                // Arithmetic instruction, reg/memory with register to either
                let op = ArithmeticOperation::of_byte((b & 0b00111000u8) / 8);
                let is_wide = b % 2 == 1;
                let d = (b / 2) % 2;
                let mod_reg_rm = bytes.next().unwrap();
                let mode = (mod_reg_rm & 0b11000000) / 64;
                let reg = Register::of_id((mod_reg_rm & 0b00111000) / 8, is_wide);
                let rm = mod_reg_rm & 0b00000111;
                if mode == 3 {
                    let rm = Register::of_id(rm, is_wide);

                    let (source, dest) = if d == 0 { (reg, rm) } else { (rm, reg) };
                    Some(Instruction::Arithmetic(ArithmeticInstruction {
                        op,
                        instruction: ArithmeticInstructionSelect::RegisterToRegister(
                            RegRegArithmetic { source, dest },
                        ),
                    }))
                } else {
                    let mem_location = Self::mode_rm_to_eaddr(mode, rm, bytes);
                    if d == 0 {
                        Some(Instruction::Arithmetic(ArithmeticInstruction {
                            op,
                            instruction: ArithmeticInstructionSelect::RegisterToMemory(
                                RegMemArithmetic {
                                    source: reg,
                                    dest: mem_location,
                                },
                            ),
                        }))
                    } else {
                        Some(Instruction::Arithmetic(ArithmeticInstruction {
                            op,
                            instruction: ArithmeticInstructionSelect::MemoryToRegister(
                                MemRegArithmetic {
                                    source: mem_location,
                                    dest: reg,
                                },
                            ),
                        }))
                    }
                }
            } else if b & 0b11111100 == 0b10000000 {
                // Immediate to register/memory
                let w = b % 2;
                let signed = (b / 2) % 2 == 1;
                let mod_reg_rm = bytes.next().unwrap();
                let mode = (mod_reg_rm & 0b11000000) / 64;
                let op = ArithmeticOperation::of_byte((mod_reg_rm & 0b00111000) / 8);
                let rm = mod_reg_rm & 0b00000111;
                let data_low = bytes.next().unwrap();
                if mode == 3 {
                    let dest = Register::of_id(rm, w == 1);
                    Some(Instruction::Arithmetic(ArithmeticInstruction {
                        op,
                        instruction: if w == 0 || signed {
                            ArithmeticInstructionSelect::ImmediateToRegisterByte(
                                dest, data_low, signed,
                            )
                        } else {
                            let data = (bytes.next().unwrap() as u16) * 256 + data_low as u16;
                            ArithmeticInstructionSelect::ImmediateToRegisterWord(dest, data, signed)
                        },
                    }))
                } else {
                    let dest = Self::mode_rm_to_eaddr(mode, rm, bytes);
                    Some(Instruction::Arithmetic(ArithmeticInstruction {
                        op,
                        instruction: if w == 0 || signed {
                            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(
                                dest, data_low, signed,
                            )
                        } else {
                            let data = (bytes.next().unwrap() as u16) * 256 + data_low as u16;
                            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(
                                dest, data, signed,
                            )
                        },
                    }))
                }
            } else if b & 0b11000110u8 == 0b00000100 {
                // Immediate to accumulator
                let w = b % 2;
                let data = bytes.next().unwrap();
                let op = ArithmeticOperation::of_byte((b & 0b00111000) / 8);
                Some(Instruction::Arithmetic(ArithmeticInstruction {
                    op,
                    instruction: if w == 0 {
                        ArithmeticInstructionSelect::ImmediateToAccByte(data)
                    } else {
                        let data = 256 * (bytes.next().unwrap() as u16) + data as u16;
                        ArithmeticInstructionSelect::ImmediateToAccWord(data)
                    },
                }))
            } else if b & 0b11111100 == 0b11100000 {
                // Loop
                let next = bytes.next().unwrap();
                let instruction = match b % 4 {
                    0 => Jump::Loopnz,
                    1 => Jump::Loopz,
                    2 => Jump::Loop,
                    3 => Jump::Jcxz,
                    b => panic!("maths fail, {} is not a remainder mod 4", b),
                };
                Some(Instruction::Jump(
                    instruction,
                    if next >= 128 {
                        (255 - next) as i8 - 1
                    } else {
                        next as i8
                    },
                ))
            } else if b & 0b11110000 == 0b01110000 {
                // Jump
                let next = bytes.next().unwrap();
                let instruction = match b % 16 {
                    0 => Jump::Jo,
                    1 => Jump::Jno,
                    2 => Jump::Jb,
                    3 => Jump::Jnb,
                    4 => Jump::Je,
                    5 => Jump::Jne,
                    6 => Jump::Jbe,
                    7 => Jump::Jnbe,
                    8 => Jump::Js,
                    9 => Jump::Jns,
                    10 => Jump::Jp,
                    11 => Jump::Jnp,
                    12 => Jump::Jl,
                    13 => Jump::Jnl,
                    14 => Jump::Jle,
                    15 => Jump::Jnle,
                    b => panic!("maths fail, {} is not a remainder mod 16", b),
                };
                Some(Instruction::Jump(
                    instruction,
                    if next >= 128 {
                        (255 - next) as i8 - 1
                    } else {
                        next as i8
                    },
                ))
            } else {
                panic!("Unrecognised instruction byte: {}", b)
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Program<T>
where
    T: AsRef<[Instruction]>,
{
    bits: u8,
    instructions: T,
}

impl<T> Display for Program<T>
where
    T: AsRef<[Instruction]>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("bits {}\n", self.bits))?;
        for i in self.instructions.as_ref().iter() {
            f.write_fmt(format_args!("{}\n", i))?;
        }
        std::fmt::Result::Ok(())
    }
}

impl<T> Program<T>
where
    T: AsRef<[Instruction]>,
{
    pub fn to_bytes(&self) -> Vec<u8> {
        if self.bits != 16 {
            panic!("Only 16-bits supported");
        }
        self.instructions
            .as_ref()
            .iter()
            .flat_map(Instruction::to_bytes)
            .collect()
    }
}

impl Program<Vec<Instruction>> {
    fn of_bytes<I>(mut bytes: I) -> Program<Vec<Instruction>>
    where
        I: Iterator<Item = u8>,
    {
        let mut output = Vec::new();

        while let Some(i) = Instruction::consume(&mut bytes) {
            println!("{}", i);
            output.push(i);
        }

        Program {
            bits: 16,
            instructions: output,
        }
    }
}

fn load_machine_code<P>(path: P) -> Vec<u8>
where
    P: AsRef<Path>,
{
    fs::read(path).unwrap()
}

#[derive(Parser)]
struct Args {
    #[arg(value_name = "COMPILED_PATH")]
    compiled_path: std::path::PathBuf,
    #[arg(value_name = "ASM_PATH")]
    asm_path: std::path::PathBuf,
}

fn main() {
    let args = Args::parse();

    let expected_bytecode = load_machine_code(args.compiled_path);
    let asm = fs::read_to_string(args.asm_path).unwrap();
    let (remaining, compiled) = assembly::program(&asm).unwrap();

    if !remaining.is_empty() {
        println!(
            "Failed to parse, as there was remaining code:\n{}",
            remaining
        );
        std::process::exit(2)
    }

    let actual_bytecode = compiled.to_bytes();
    if expected_bytecode != actual_bytecode {
        println!(
            "Expected: {:?}\nActual:   {:?}",
            expected_bytecode, actual_bytecode
        );
        std::process::exit(1)
    }

    let disassembled = Program::of_bytes(expected_bytecode.iter().cloned());

    if disassembled != compiled {
        println!("Program failed to disassemble back to the compiled version. Compiled:\n{}\nDisassembled again:\n{}", compiled, disassembled);
        std::process::exit(3)
    }
}

#[cfg(test)]
mod test_program {
    use crate::{
        register::{GeneralRegister, Register, RegisterSubset},
        ImmediateToRegister, Instruction, MemRegMove, Program,
    };

    use super::assembly::program;

    #[test]
    fn test_programs_with_different_instruction_sequences_are_not_equal() {
        let program1 = Program {
            bits: 64,
            instructions: vec![],
        };
        let program2 = Program {
            bits: 64,
            instructions: vec![Instruction::ImmediateToRegister(ImmediateToRegister::Byte(
                Register::General(GeneralRegister::D, RegisterSubset::All),
                1,
            ))],
        };

        assert_ne!(program1, program2);
    }

    #[test]
    fn test_programs_with_identical_instruction_sequences_are_equal() {
        let program1 = Program {
            bits: 64,
            instructions: vec![Instruction::ImmediateToRegister(ImmediateToRegister::Byte(
                Register::General(GeneralRegister::D, RegisterSubset::All),
                1,
            ))],
        };
        let program2 = Program {
            bits: 64,
            instructions: vec![Instruction::ImmediateToRegister(ImmediateToRegister::Byte(
                Register::General(GeneralRegister::D, RegisterSubset::All),
                1,
            ))],
        };

        assert_eq!(program1, program2);
    }

    fn test_parser<T>(input_asm: &str, input_bytecode: T)
    where
        T: AsRef<[u8]>,
    {
        let (remaining, parsed) = program(input_asm).unwrap();
        assert_eq!(remaining, "");
        assert_eq!(parsed.bits, 16);

        for (i, (actual, expected)) in parsed
            .to_bytes()
            .iter()
            .zip(input_bytecode.as_ref().iter())
            .enumerate()
        {
            if actual != expected {
                panic!(
                    "Failed assertion: expected {}, got {}, at position {}",
                    expected, actual, i
                )
            }
        }
    }

    fn test_disassembler<T>(input_asm: &str, input_bytecode: T)
    where
        T: AsRef<[u8]>,
    {
        let disassembled = Program::of_bytes(input_bytecode.as_ref().iter().cloned());

        let (remaining, pre_compiled) = program(&input_asm).unwrap();
        assert_eq!(remaining, "");

        if disassembled != pre_compiled {
            for (theirs, ours) in disassembled
                .instructions
                .iter()
                .zip(pre_compiled.instructions.iter())
            {
                if theirs != ours {
                    println!(
                        "Different instruction. Ours: {ours} ({:?}). Theirs: {theirs} ({:?}).",
                        ours.to_bytes(),
                        theirs.to_bytes()
                    );
                }
            }
            panic!(
                "Failed assertion. Our disassembly:\n{}\nReference:\n{}",
                disassembled, pre_compiled
            );
        }
    }

    #[test]
    fn test_register_register_mov_parser() {
        let input_asm = include_str!(
            "../computer_enhance/perfaware/part1/listing_0037_single_register_mov.asm"
        );
        let input_bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0037_single_register_mov");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_register_mov_disassembler() {
        let bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0037_single_register_mov");
        let asm = include_str!(
            "../computer_enhance/perfaware/part1/listing_0037_single_register_mov.asm"
        );
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_register_register_many_mov_parser() {
        let input_asm =
            include_str!("../computer_enhance/perfaware/part1/listing_0038_many_register_mov.asm");
        let input_bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0038_many_register_mov");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_register_many_mov_disassembler() {
        let bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0038_many_register_mov");
        let asm =
            include_str!("../computer_enhance/perfaware/part1/listing_0038_many_register_mov.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_register_more_mov_parser() {
        let input_asm =
            include_str!("../computer_enhance/perfaware/part1/listing_0039_more_movs.asm");
        let input_bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0039_more_movs");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_more_mov_disassembler() {
        let bytecode = include_bytes!("../computer_enhance/perfaware/part1/listing_0039_more_movs");
        let asm = include_str!("../computer_enhance/perfaware/part1/listing_0039_more_movs.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_register_challenge_movs_parser() {
        let input_asm =
            include_str!("../computer_enhance/perfaware/part1/listing_0040_challenge_movs.asm");
        let input_bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0040_challenge_movs");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_challenge_movs_disassembler() {
        let bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0040_challenge_movs");
        let asm =
            include_str!("../computer_enhance/perfaware/part1/listing_0040_challenge_movs.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_add_sub_cmp_jnz_parser() {
        let input_asm =
            include_str!("../computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz.asm");
        let input_bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_add_sub_cmp_jnz_disassembler() {
        let bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz");
        let asm =
            include_str!("../computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn mem_reg_move_to_bytes() {
        let i = Instruction::MemRegMove(MemRegMove {
            source: crate::EffectiveAddress::BasePointer(0),
            dest: Register::General(GeneralRegister::D, RegisterSubset::All),
        });
        assert_eq!(i.to_bytes(), vec![139, 86, 0]);
    }
}
