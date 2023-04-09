mod assembly;
mod register;

use std::{
    fmt::{Display, Write},
    fs,
    path::Path,
};

use clap::Parser;
use register::{ByteRegisterSubset, Register, RegisterSubset};

#[derive(Eq, PartialEq)]
pub struct RegRegMove {
    source: Register,
    dest: Register,
}

#[derive(Eq, PartialEq)]
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

#[derive(Eq, PartialEq)]
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

#[derive(Eq, PartialEq)]
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

#[derive(Eq, PartialEq)]
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
            EffectiveAddress::SpecifiedIn(register) => f.write_fmt(format_args!("{}", register)),
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

#[derive(Eq, PartialEq)]
pub struct RegMemMove {
    source: Register,
    dest: EffectiveAddress,
}

#[derive(Eq, PartialEq)]
pub struct MemRegMove {
    source: EffectiveAddress,
    dest: Register,
}

#[derive(Eq, PartialEq)]
pub enum ImmediateToRegister {
    Byte(Register, u8),
    Wide(Register, u16),
}

#[derive(Eq, PartialEq)]
pub enum Instruction {
    RegRegMove(RegRegMove),
    RegMemMove(RegMemMove),
    MemRegMove(MemRegMove),
    ImmediateToRegister(ImmediateToRegister),
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
            Instruction::ImmediateToRegister(ImmediateToRegister::Byte(dest, value)) => {
                f.write_fmt(format_args!("mov {}, {}", dest, value))
            }
            Instruction::ImmediateToRegister(ImmediateToRegister::Wide(dest, value)) => {
                f.write_fmt(format_args!("mov {}, {}", dest, value))
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
                result.push(mode + reg * 8 + rm);
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
                result.push(mode + reg * 8 + rm);
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
                result.push(mode + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)) => {
                let mode = 0u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode + reg * 8 + rm);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, offset)) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU16(source_dest, offset)) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::Bx(WithOffset::Basic(())) => {
                let mode = 0u8;
                let rm = 7u8;
                result.push(mode + reg * 8 + rm);
            }
            EffectiveAddress::Bx(WithOffset::WithU8((), offset)) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = 7u8;
                result.push(mode + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::Bx(WithOffset::WithU16((), offset)) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = 7u8;
                result.push(mode + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::Direct(address) => {
                result.reserve_exact(2);
                let mode = 0u8;
                let rm = 6u8;
                result.push(mode + reg * 8 + rm);
                result.push((address % 256) as u8);
                result.push((address / 256) as u8);
            }
            EffectiveAddress::BasePointer(offset) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = 6u8;
                result.push(mode + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::BasePointerWide(offset) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = 6u8;
                result.push(mode + reg * 8 + rm);
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
                        let displacement_high = if rm == 6 || mode > 0 {
                            bytes.next().expect("required an 8-bit displacement")
                        } else {
                            0
                        };
                        let displacement_low = if (rm == 6 && mode == 0) || mode == 2 {
                            let low = bytes.next().expect("required a 16-bit displacement");
                            (displacement_high as u16) * 256 + (low as u16)
                        } else {
                            0
                        };

                        let mem_location = if rm < 4 {
                            match mode {
                                0 => EffectiveAddress::Sum(WithOffset::Basic((base, source_dest))),
                                1 => EffectiveAddress::Sum(WithOffset::WithU8(
                                    (base, source_dest),
                                    displacement_high,
                                )),
                                2 => EffectiveAddress::Sum(WithOffset::WithU16(
                                    (base, source_dest),
                                    displacement_low,
                                )),
                                _ => panic!("Maths is wrong, got bad mode: {}", mode),
                            }
                        } else if rm < 6 {
                            match mode {
                                0 => EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)),
                                1 => EffectiveAddress::SpecifiedIn(WithOffset::WithU8(
                                    source_dest,
                                    displacement_high,
                                )),
                                2 => EffectiveAddress::SpecifiedIn(WithOffset::WithU16(
                                    source_dest,
                                    displacement_low,
                                )),
                                _ => panic!("Maths is wrong, got bad mode: {}", mode),
                            }
                        } else if rm == 6 {
                            match mode {
                                0 => EffectiveAddress::Direct(displacement_low),
                                1 => EffectiveAddress::BasePointer(displacement_high),
                                2 => EffectiveAddress::BasePointerWide(displacement_low),
                                _ => panic!("Maths is wrong, got bad mode: {}", mode),
                            }
                        } else {
                            assert!(rm == 7);
                            match mode {
                                0 => EffectiveAddress::Bx(WithOffset::Basic(())),
                                1 => {
                                    EffectiveAddress::Bx(WithOffset::WithU8((), displacement_high))
                                }
                                2 => {
                                    EffectiveAddress::Bx(WithOffset::WithU16((), displacement_low))
                                }
                                _ => panic!("Maths is wrong, got bad mode: {}", mode),
                            }
                        };

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
            } else {
                panic!("Unrecognised instruction byte: {}", b)
            }
        } else {
            None
        }
    }
}

pub struct Program<T>
where
    T: AsRef<[Instruction]>,
{
    bits: u8,
    instructions: T,
}

impl<T> PartialEq for Program<T>
where
    T: AsRef<[Instruction]>,
{
    fn eq(&self, other: &Self) -> bool {
        if self.bits != other.bits {
            return false;
        };
        let iter1 = self.instructions.as_ref();
        let iter2 = self.instructions.as_ref();
        if iter1.len() != iter2.len() {
            return false;
        }

        if !iter1.iter().zip(iter2.iter()).all(|(x, y)| x == y) {
            return false;
        }

        true
    }
}

impl<T> Eq for Program<T> where T: AsRef<[Instruction]> {}

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
    use crate::Program;

    use super::assembly::program;

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
    fn test_register_register_more_mov_parser() {
        let input_asm =
            include_str!("../computer_enhance/perfaware/part1/listing_0039_more_movs.asm");
        let input_bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0039_more_movs");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_register_more_mov_disassembler() {
        let bytecode = include_bytes!("../computer_enhance/perfaware/part1/listing_0039_more_movs");
        let asm = include_str!("../computer_enhance/perfaware/part1/listing_0039_more_movs.asm");
        test_disassembler(asm, bytecode)
    }
}
