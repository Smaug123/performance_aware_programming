use std::fmt::Display;

use crate::{move_instruction::{MoveInstruction, RegRegMove, RegMemMove, ImmediateToRegister, MemoryToAccumulator, AccumulatorToMemory, ImmediateToRegisterOrMemory, MemRegMove}, arithmetic_instruction::{ArithmeticInstruction, ArithmeticInstructionSelect, ArithmeticOperation, RegMemArithmetic, RegRegArithmetic, MemRegArithmetic}, jump_instruction::Jump, trivia_instruction::TriviaInstruction, register::Register, effective_address::EffectiveAddress};

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum Instruction<InstructionOffset> {
    Move(MoveInstruction),
    /// Perform arithmetic
    Arithmetic(ArithmeticInstruction),
    Jump(Jump, InstructionOffset),
    /// An irrelevant instruction.
    Trivia(TriviaInstruction<InstructionOffset>),
}

impl<InstructionOffset> Display for Instruction<InstructionOffset>
where
    InstructionOffset: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Move(instruction) => {
                f.write_fmt(format_args!("{}", instruction))
            }
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
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(addr, data) => {
                        f.write_fmt(format_args!("{}, {}", addr, data))
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterByte(addr, data, signed) => {
                        if *signed {
                            f.write_fmt(format_args!("{}, {} ; signed byte", addr, *data))
                        } else {
                            f.write_fmt(format_args!("{}, {}", addr, data))
                        }
                    }
                    ArithmeticInstructionSelect::ImmediateToRegisterWord(addr, data, signed) => {
                        if *signed {
                            f.write_fmt(format_args!("{}, {} ; signed word", addr, *data))
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
            Instruction::Trivia(trivia) => match trivia {
                TriviaInstruction::Label(l) => f.write_fmt(format_args!("{}:", l)),
            },
        }
    }
}

impl<'a> Instruction<&'a str> {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            Instruction::Move(mov) => mov.to_bytes(),
            Instruction::Arithmetic(instruction) => instruction.to_bytes(),
            Instruction::Jump(instruction, _) => {
                vec![
                    match instruction {
                        Jump::Je => 0b01110100,
                        Jump::Jl => 0b01111100,
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
                    },
                    // Placeholder destination which will be filled in later
                    0,
                ]
            }

            Instruction::Trivia(_) => vec![],
        }
    }
}

impl Instruction<i8> {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            Instruction::Move(mov) => mov.to_bytes(),
            Instruction::Arithmetic(instruction) => instruction.to_bytes(),
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

            Instruction::Trivia(_) => vec![],
        }
    }

    pub fn consume<I>(bytes: &mut I) -> Option<Instruction<i8>>
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
                        Some(Instruction::Move(MoveInstruction::RegRegMove(instruction)))
                    } else {
                        let mem_location = EffectiveAddress::of_mode_rm(mode, rm, bytes);
                        if d == 0 {
                            Some(Instruction::Move(MoveInstruction::RegMemMove(RegMemMove {
                                source: reg,
                                dest: mem_location,
                            })))
                        } else {
                            Some(Instruction::Move(MoveInstruction::MemRegMove(MemRegMove {
                                dest: reg,
                                source: mem_location,
                            })))
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
                    Some(Instruction::Move(MoveInstruction::ImmediateToRegister(ImmediateToRegister::Wide(
                        reg,
                        next_low + 256 * next_high,
                    ))))
                } else {
                    let reg = Register::of_id(b % 8, false);
                    let next_low = bytes.next().unwrap();
                    Some(Instruction::Move(MoveInstruction::ImmediateToRegister(ImmediateToRegister::Byte(
                        reg, next_low,
                    ))))
                }
            } else if (b & 0b11111110) == 0b10100000 {
                // Memory to accumulator
                let w = b % 2;
                let addr_low = bytes.next().unwrap() as u16;
                let addr_high = bytes.next().unwrap() as u16 * 256;
                Some(Instruction::Move(MoveInstruction::MemoryToAccumulator(MemoryToAccumulator {
                    address: addr_high + addr_low,
                    is_wide: w == 1,
                })))
            } else if (b & 0b11111110) == 0b10100010 {
                // Accumulator to memory
                let w = b % 2;
                let addr_low = bytes.next().unwrap() as u16;
                let addr_high = bytes.next().unwrap() as u16 * 256;
                Some(Instruction::Move(MoveInstruction::AccumulatorToMemory(AccumulatorToMemory {
                    address: addr_high + addr_low,
                    is_wide: w == 1,
                })))
            } else if (b & 0b11111110) == 0b11000110 {
                // Immediate to register/memory
                let w = b % 2;
                let mod_reg_rm = bytes.next().unwrap();
                let mode = (mod_reg_rm & 0b11000000) / 64;
                let reg = (mod_reg_rm & 0b00111000) / 8;
                let rm = mod_reg_rm & 0b00000111;
                assert_eq!(reg, 0);
                let dest = EffectiveAddress::of_mode_rm(mode, rm, bytes);

                let data_low = bytes.next().unwrap();
                if w == 1 {
                    let data_high = bytes.next().unwrap() as u16 * 256;
                    Some(Instruction::Move(MoveInstruction::ImmediateToRegisterOrMemory(
                        ImmediateToRegisterOrMemory::Word(dest, data_high + data_low as u16),
                    )))
                } else {
                    Some(Instruction::Move(MoveInstruction::ImmediateToRegisterOrMemory(
                        ImmediateToRegisterOrMemory::Byte(dest, data_low),
                    )))
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
                    let mem_location = EffectiveAddress::of_mode_rm(mode, rm, bytes);
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
                if mode == 3 {
                    let data_low = bytes.next().unwrap();
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
                    let dest = EffectiveAddress::of_mode_rm(mode, rm, bytes);
                    let data_low = bytes.next().unwrap();
                    Some(Instruction::Arithmetic(ArithmeticInstruction {
                        op,
                        instruction: if w == 0 || signed {
                            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(
                                dest, data_low, signed,
                            )
                        } else {
                            let data = (bytes.next().unwrap() as u16) * 256 + data_low as u16;
                            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(dest, data)
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
