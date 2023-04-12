use std::{fs, path::Path};

use clap::Parser;
use sim_8086::instruction::Instruction;
use sim_8086::program::Program;

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

fn program_equal_ignoring_labels<A, B>(
    p1: &Program<Vec<Instruction<A>>, A>,
    p2: &Program<Vec<Instruction<B>>, B>,
) -> bool
where
    A: PartialEq,
{
    if p1.bits != p2.bits {
        return false;
    }

    let without_trivia_1 = p1
        .instructions
        .iter()
        .filter(|i| !matches!(i, Instruction::Trivia(_)));
    let mut without_trivia_2 = p1
        .instructions
        .iter()
        .filter(|i| !matches!(i, Instruction::Trivia(_)));

    for i1 in without_trivia_1 {
        if let Some(i2) = without_trivia_2.next() {
            if i1 != i2 {
                return false;
            }
        }
    }

    if without_trivia_2.next().is_some() {
        return false;
    }

    true
}

fn main() {
    let args = Args::parse();

    let expected_bytecode = load_machine_code(args.compiled_path);
    let asm = fs::read_to_string(args.asm_path).unwrap();
    let (remaining, compiled) = sim_8086::assembly::program(&asm).unwrap();

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
        println!("Disassembled and compiled versions do not produce the same bytes. From disassembly:\n{}\nFrom assembling the input asm:\n{}", disassembled, compiled);
        std::process::exit(3)
    }

    if !program_equal_ignoring_labels(&disassembled, &compiled) {
        println!("Program failed to disassemble back to the compiled version. Compiled:\n{}\nDisassembled again:\n{}", compiled, disassembled);
        std::process::exit(4)
    }
}
