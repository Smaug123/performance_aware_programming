use std::{collections::HashMap, fmt::Display, marker::PhantomData};

use crate::{instruction::Instruction, trivia_instruction::TriviaInstruction};

#[derive(Debug, Eq, PartialEq)]
pub struct Program<T, InstructionOffset>
where
    T: AsRef<[Instruction<InstructionOffset>]>,
{
    pub bits: u8,
    pub instructions: T,
    pub offset: PhantomData<InstructionOffset>,
}

impl<T, InstructionOffset> Display for Program<T, InstructionOffset>
where
    T: AsRef<[Instruction<InstructionOffset>]>,
    InstructionOffset: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("bits {}\n", self.bits))?;
        for i in self.instructions.as_ref().iter() {
            f.write_fmt(format_args!("{}\n", i))?;
        }
        std::fmt::Result::Ok(())
    }
}

impl<'a, T> Program<T, &'a str>
where
    T: AsRef<[Instruction<&'a str>]>,
{
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::new();
        if self.bits != 16 {
            panic!("Only 16-bits supported");
        }
        let mut labels = HashMap::new();
        for (counter, instruction) in self.instructions.as_ref().iter().enumerate() {
            if let Instruction::Trivia(TriviaInstruction::Label(s)) = instruction {
                if let Some(s) = labels.insert(*s, counter) {
                    panic!("same label twice: {}", s)
                }
            }
        }

        let mut instruction_boundaries = vec![0; self.instructions.as_ref().len()];

        for (counter, instruction) in self.instructions.as_ref().iter().enumerate() {
            let new_bytes = instruction.to_bytes();
            result.extend(new_bytes);
            instruction_boundaries[counter] = result.len();
        }

        for (counter, instruction) in self.instructions.as_ref().iter().enumerate() {
            if let Instruction::Jump(_, offset) = instruction {
                let desired_target_instruction_number = match labels.get(offset) {
                    Some(s) => *s,
                    None => panic!("Tried to jump to label, but was not present: '{}'", offset),
                };
                let required_jump =
                    (instruction_boundaries[desired_target_instruction_number] as i64
                        - instruction_boundaries[counter] as i64) as i8;
                let required_jump = if required_jump < 0 {
                    255 - (-required_jump as u8) + 1
                } else {
                    required_jump as u8
                };
                result[instruction_boundaries[counter] - 1] = required_jump;
            }
        }

        result
    }
}

impl<T> Program<T, i8>
where
    T: AsRef<[Instruction<i8>]>,
{
    pub fn to_bytes(&self) -> Vec<u8> {
        if self.bits != 16 {
            panic!("Only 16-bits supported");
        }
        self.instructions
            .as_ref()
            .iter()
            .flat_map(Instruction::<i8>::to_bytes)
            .collect()
    }
}

impl Program<Vec<Instruction<i8>>, i8> {
    pub fn of_bytes<I>(mut bytes: I) -> Program<Vec<Instruction<i8>>, i8>
    where
        I: Iterator<Item = u8>,
    {
        let mut output = Vec::new();

        while let Some(i) = Instruction::consume(&mut bytes) {
            // println!("{}", i);
            output.push(i);
        }

        Program {
            bits: 16,
            instructions: output,
            offset: PhantomData,
        }
    }
}

impl<'a, T, U> PartialEq<Program<U, &'a str>> for Program<T, i8>
where
    T: AsRef<[Instruction<i8>]>,
    U: AsRef<[Instruction<&'a str>]>,
{
    fn eq(&self, other: &Program<U, &'a str>) -> bool {
        Program::<T, i8>::to_bytes(self) == Program::<U, &'a str>::to_bytes(other)
    }
}
