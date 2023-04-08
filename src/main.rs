mod assembly;
mod register;

use std::{fmt::Display, fs, path::Path};

use clap::Parser;
use register::{ByteRegisterSubset, Register, RegisterSubset};

#[derive(Eq, PartialEq)]
pub struct MoveInstruction {
    source: Register,
    dest: Register,
}

#[derive(Eq, PartialEq)]
pub enum Instruction {
    Move(MoveInstruction),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Move(mov) => f.write_fmt(format_args!("mov {}, {}", mov.dest, mov.source)),
        }
    }
}

impl Instruction {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            Instruction::Move(mov) => {
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
}

#[cfg(test)]
mod test_program {
    use super::assembly::program;

    #[test]
    fn test_parser() {
        let input_asm = include_str!(
            "../computer_enhance/perfaware/part1/listing_0037_single_register_mov.asm"
        );
        let input_bytecode =
            include_bytes!("../computer_enhance/perfaware/part1/listing_0037_single_register_mov");
        let (remaining, parsed) = program(input_asm).unwrap();
        assert_eq!(remaining, "");
        assert_eq!(parsed.bits, 16);
        for (i, (actual, expected)) in parsed
            .to_bytes()
            .iter()
            .zip(input_bytecode.iter())
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
}
