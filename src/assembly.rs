use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, digit1, line_ending, multispace0, not_line_ending, one_of},
    combinator::map_res,
    multi::many0,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::{
    register::{ByteRegisterSubset, GeneralRegister, Register, RegisterSubset, SpecialRegister},
    AccumulatorToMemory, ArithmeticInstruction, ArithmeticInstructionSelect, ArithmeticOperation,
    Base, EffectiveAddress, ImmediateToRegister, ImmediateToRegisterOrMemory, Instruction, Jump,
    MemRegMove, MemoryToAccumulator, Program, RegMemMove, RegRegMove, SourceDest,
    TriviaInstruction, WithOffset,
};

fn comment(input: &str) -> IResult<&str, &str> {
    preceded(char(';'), terminated(not_line_ending, line_ending))(input)
}

fn ws<'a, F, O, E: nom::error::ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn bracketed<'a, F, O, E: nom::error::ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    preceded(char('['), terminated(inner, char(']')))
}

fn argument_sep(input: &str) -> IResult<&str, ()> {
    map_res(ws(char(',')), |_| Ok::<_, ()>(()))(input)
}

fn bits(input: &str) -> IResult<&str, u8> {
    let p = preceded(tag("bits "), terminated(digit1, line_ending));
    map_res(p, str::parse)(input)
}

fn general_register(input: &str) -> IResult<&str, GeneralRegister> {
    map_res(one_of("abcd"), |c| match c {
        'a' => Ok::<_, ()>(GeneralRegister::A),
        'b' => Ok(GeneralRegister::B),
        'c' => Ok(GeneralRegister::C),
        'd' => Ok(GeneralRegister::D),
        _ => panic!("cannot hit"),
    })(input)
}

fn byte_register_subset(input: &str) -> IResult<&str, ByteRegisterSubset> {
    map_res(one_of("hl"), |c| match c {
        'h' => Ok::<_, ()>(ByteRegisterSubset::High),
        'l' => Ok(ByteRegisterSubset::Low),
        _ => panic!("cannot hit"),
    })(input)
}

fn register_subset(input: &str) -> IResult<&str, RegisterSubset> {
    alt((
        map_res(byte_register_subset, |x| {
            Ok::<_, ()>(RegisterSubset::Subset(x))
        }),
        map_res(char('x'), |_| Ok::<_, ()>(RegisterSubset::All)),
    ))(input)
}

fn special_register(input: &str) -> IResult<&str, SpecialRegister> {
    alt((
        map_res(tag("si"), |_| Ok::<_, ()>(SpecialRegister::SourceIndex)),
        map_res(tag("bp"), |_| Ok::<_, ()>(SpecialRegister::BasePointer)),
        map_res(tag("sp"), |_| Ok::<_, ()>(SpecialRegister::StackPointer)),
        map_res(tag("di"), |_| Ok::<_, ()>(SpecialRegister::DestIndex)),
    ))(input)
}

fn wide_register(input: &str) -> IResult<&str, Register> {
    alt((
        map_res(
            tuple((general_register, register_subset)),
            |(reg, sub)| match sub {
                RegisterSubset::Subset(_) => Err(()),
                RegisterSubset::All => Ok::<_, ()>(Register::General(reg, sub)),
            },
        ),
        map_res(special_register, |r| Ok::<_, ()>(Register::Special(r))),
    ))(input)
}

fn byte_register(input: &str) -> IResult<&str, Register> {
    map_res(
        tuple((general_register, register_subset)),
        |(reg, sub)| match sub {
            RegisterSubset::Subset(_) => Ok::<_, ()>(Register::General(reg, sub)),
            RegisterSubset::All => Err(()),
        },
    )(input)
}

fn register(input: &str) -> IResult<&str, Register> {
    alt((byte_register, wide_register))(input)
}

fn reg_reg_move_instruction(input: &str) -> IResult<&str, RegRegMove> {
    map_res(
        preceded(
            tag("mov "),
            tuple((register, argument_sep, register, line_ending)),
        ),
        |(dest, _, source, _)| Ok::<_, ()>(RegRegMove { dest, source }),
    )(input)
}

fn direct_offset(input: &str) -> IResult<&str, u16> {
    bracketed(map_res(digit1, str::parse))(input)
}

fn base(input: &str) -> IResult<&str, Base> {
    alt((
        map_res(tag("bx"), |_| Ok::<_, ()>(Base::Bx)),
        map_res(tag("bp"), |_| Ok::<_, ()>(Base::Bp)),
    ))(input)
}

fn source_dest(input: &str) -> IResult<&str, SourceDest> {
    alt((
        map_res(tag("si"), |_| Ok::<_, ()>(SourceDest::Source)),
        map_res(tag("di"), |_| Ok::<_, ()>(SourceDest::Dest)),
    ))(input)
}

fn absolute_u8(input: &str) -> IResult<&str, u8> {
    map_res(
        alt((digit1, preceded(tag("byte "), digit1))),
        str::parse::<u8>,
    )(input)
}

fn absolute_u16(input: &str) -> IResult<&str, u16> {
    map_res(
        alt((digit1, preceded(tag("word "), digit1))),
        str::parse::<u16>,
    )(input)
}

fn negative_u8(input: &str) -> IResult<&str, u8> {
    map_res(
        preceded(ws(char('-')), alt((digit1, preceded(tag("byte "), digit1)))),
        |x| str::parse::<u8>(x).map(|x| 255 - x + 1),
    )(input)
}

fn negative_u16(input: &str) -> IResult<&str, u16> {
    map_res(
        preceded(ws(char('-')), alt((digit1, preceded(tag("word "), digit1)))),
        |x| str::parse::<u16>(x).map(|x| 65535 - x + 1),
    )(input)
}

fn literal_u8(input: &str) -> IResult<&str, u8> {
    alt((absolute_u8, negative_u8))(input)
}

fn literal_u16(input: &str) -> IResult<&str, u16> {
    alt((absolute_u16, negative_u16))(input)
}

fn effective_address(input: &str) -> IResult<&str, EffectiveAddress> {
    alt((
        // Sum, no offset
        map_res(
            bracketed(tuple((base, ws(char('+')), source_dest))),
            |(base, _, source_dest)| {
                Ok::<_, ()>(EffectiveAddress::Sum(WithOffset::Basic((
                    base,
                    source_dest,
                ))))
            },
        ),
        // Sum, with offset
        map_res(
            bracketed(tuple((
                base,
                ws(char('+')),
                source_dest,
                alt((preceded(ws(char('+')), absolute_u8), negative_u8)),
            ))),
            |(base, _, source_dest, offset)| {
                Ok::<_, ()>(EffectiveAddress::Sum(WithOffset::WithU8(
                    (base, source_dest),
                    offset,
                )))
            },
        ),
        map_res(
            bracketed(tuple((
                base,
                ws(char('+')),
                source_dest,
                alt((preceded(ws(char('+')), absolute_u16), negative_u16)),
            ))),
            |(base, _, source_dest, offset)| {
                Ok::<_, ()>(EffectiveAddress::Sum(WithOffset::WithU16(
                    (base, source_dest),
                    offset,
                )))
            },
        ),
        // Lookup
        map_res(source_dest, |source_dest| {
            Ok::<_, ()>(EffectiveAddress::SpecifiedIn(WithOffset::Basic(
                source_dest,
            )))
        }),
        map_res(
            bracketed(tuple((
                source_dest,
                alt((preceded(ws(char('+')), absolute_u8), negative_u8)),
            ))),
            |(source_dest, offset)| {
                Ok::<_, ()>(EffectiveAddress::SpecifiedIn(WithOffset::WithU8(
                    source_dest,
                    offset,
                )))
            },
        ),
        map_res(
            bracketed(tuple((
                source_dest,
                alt((preceded(ws(char('+')), absolute_u16), negative_u16)),
            ))),
            |(source_dest, offset)| {
                Ok::<_, ()>(EffectiveAddress::SpecifiedIn(WithOffset::WithU16(
                    source_dest,
                    offset,
                )))
            },
        ),
        // Offset from BX
        map_res(bracketed(tag("bx")), |_| {
            Ok::<_, ()>(EffectiveAddress::Bx(WithOffset::Basic(())))
        }),
        map_res(
            bracketed(tuple((
                tag("bx"),
                alt((preceded(ws(char('+')), absolute_u8), negative_u8)),
            ))),
            |(_, offset)| Ok::<_, ()>(EffectiveAddress::Bx(WithOffset::WithU8((), offset))),
        ),
        map_res(
            bracketed(tuple((
                tag("bx"),
                alt((preceded(ws(char('+')), absolute_u16), negative_u16)),
            ))),
            |(_, offset)| Ok::<_, ()>(EffectiveAddress::Bx(WithOffset::WithU16((), offset))),
        ),
        // Direct memory address
        map_res(direct_offset, |offset| {
            Ok::<_, ()>(EffectiveAddress::Direct(offset))
        }),
        // Offset from base pointer
        map_res(
            bracketed(tuple((
                tag("bp"),
                alt((preceded(ws(char('+')), absolute_u8), negative_u8)),
            ))),
            |(_, offset)| Ok::<_, ()>(EffectiveAddress::BasePointer(offset)),
        ),
        map_res(
            bracketed(tuple((
                tag("bp"),
                alt((preceded(ws(char('+')), absolute_u16), negative_u16)),
            ))),
            |(_, offset)| Ok::<_, ()>(EffectiveAddress::BasePointerWide(offset)),
        ),
        // Specific support for [bp], which can't be represented as a simple instruction
        map_res(bracketed(tag("bp")), |_| {
            Ok::<_, ()>(EffectiveAddress::BasePointer(0))
        }),
    ))(input)
}

fn reg_mem_move_instruction(input: &str) -> IResult<&str, RegMemMove> {
    map_res(
        preceded(
            tag("mov "),
            tuple((
                terminated(effective_address, argument_sep),
                terminated(register, line_ending),
            )),
        ),
        |(address, register)| {
            Ok::<_, ()>(RegMemMove {
                dest: address,
                source: register,
            })
        },
    )(input)
}

fn mem_reg_move_instruction(input: &str) -> IResult<&str, MemRegMove> {
    map_res(
        preceded(
            tag("mov "),
            tuple((
                terminated(register, argument_sep),
                terminated(effective_address, line_ending),
            )),
        ),
        |(register, address)| {
            Ok::<_, ()>(MemRegMove {
                dest: register,
                source: address,
            })
        },
    )(input)
}

fn immediate_wide(input: &str) -> IResult<&str, (Register, u16)> {
    tuple((
        terminated(wide_register, argument_sep),
        map_res(literal_u16, Ok::<_, ()>),
    ))(input)
}

fn immediate_byte(input: &str) -> IResult<&str, (Register, u8)> {
    tuple((
        terminated(byte_register, argument_sep),
        map_res(literal_u8, Ok::<_, ()>),
    ))(input)
}

fn immediate_to_register_instruction(input: &str) -> IResult<&str, ImmediateToRegister> {
    map_res(
        preceded(
            tag("mov "),
            alt((
                terminated(
                    map_res(immediate_wide, |(register, x)| {
                        Ok::<_, ()>((register, Err(x)))
                    }),
                    line_ending,
                ),
                terminated(
                    map_res(immediate_byte, |(register, x)| {
                        Ok::<_, ()>((register, Ok(x)))
                    }),
                    line_ending,
                ),
            )),
        ),
        |(register, contents)| {
            Ok::<_, ()>(match contents {
                Ok(contents) => {
                    if register.is_wide() {
                        ImmediateToRegister::Wide(register, contents as u16)
                    } else {
                        ImmediateToRegister::Byte(register, contents)
                    }
                }
                Err(contents) => ImmediateToRegister::Wide(register, contents),
            })
        },
    )(input)
}

fn immediate_to_memory_instruction(input: &str) -> IResult<&str, ImmediateToRegisterOrMemory> {
    map_res(
        tuple((
            terminated(preceded(tag("mov "), effective_address), argument_sep),
            alt((
                map_res(literal_u8, |x| Ok::<_, ()>(Ok(x))),
                map_res(literal_u16, |x| Ok::<_, ()>(Err(x))),
            )),
        )),
        |(addr, x)| {
            Ok::<_, ()>(match x {
                Ok(b) => ImmediateToRegisterOrMemory::Byte(addr, b),
                Err(b) => ImmediateToRegisterOrMemory::Word(addr, b),
            })
        },
    )(input)
}

fn memory_to_accumulator_instruction(input: &str) -> IResult<&str, MemoryToAccumulator> {
    map_res(
        preceded(
            tag("mov a"),
            tuple((
                terminated(alt((char('h'), char('x'))), argument_sep),
                bracketed(literal_u16),
            )),
        ),
        |(acc, address)| {
            let is_wide = acc == 'x';
            Ok::<_, ()>(MemoryToAccumulator { address, is_wide })
        },
    )(input)
}

fn accumulator_to_memory_instruction(input: &str) -> IResult<&str, AccumulatorToMemory> {
    map_res(
        preceded(
            tag("mov "),
            tuple((
                terminated(bracketed(literal_u16), tuple((argument_sep, char('a')))),
                alt((char('h'), char('x'))),
            )),
        ),
        |(address, acc)| {
            let is_wide = acc == 'x';
            Ok::<_, ()>(AccumulatorToMemory { address, is_wide })
        },
    )(input)
}

fn arithmetic_op(input: &str) -> IResult<&str, ArithmeticOperation> {
    alt((
        map_res(tag("add"), |_| Ok::<_, ()>(ArithmeticOperation::Add)),
        map_res(tag("sub"), |_| Ok::<_, ()>(ArithmeticOperation::Sub)),
        map_res(tag("cmp"), |_| Ok::<_, ()>(ArithmeticOperation::Cmp)),
        map_res(tag("acd"), |_| {
            Ok::<_, ()>(ArithmeticOperation::AddWithCarry)
        }),
        map_res(tag("sbb"), |_| {
            Ok::<_, ()>(ArithmeticOperation::SubWithBorrow)
        }),
        map_res(tag("and"), |_| Ok::<_, ()>(ArithmeticOperation::And)),
        map_res(tag("xor"), |_| Ok::<_, ()>(ArithmeticOperation::Xor)),
        map_res(tag("or"), |_| Ok::<_, ()>(ArithmeticOperation::Or)),
    ))(input)
}

fn arithmetic_select(input: &str) -> IResult<&str, ArithmeticInstructionSelect> {
    alt((
        map_res(
            preceded(
                char('a'),
                tuple((
                    terminated(alt((char('l'), char('x'))), argument_sep),
                    alt((
                        // Order is important here.
                        map_res(literal_u8, |x| Ok::<_, ()>(Err(x))),
                        map_res(literal_u16, |x| Ok::<_, ()>(Ok(x))),
                    )),
                )),
            ),
            |(which_acc, data)| match (which_acc, data) {
                ('x', Ok(wide)) => Ok(ArithmeticInstructionSelect::ImmediateToAccWord(wide)),
                ('l', Ok(_)) => Err(()),
                ('x', Err(byte)) => {
                    Ok(ArithmeticInstructionSelect::ImmediateToAccWord(byte as u16))
                }
                ('l', Err(byte)) => Ok(ArithmeticInstructionSelect::ImmediateToAccByte(byte)),
                (_, _) => panic!("can't hit, bad char {}", which_acc),
            },
        ),
        map_res(
            tuple((terminated(register, argument_sep), register)),
            |(dest, source)| {
                Ok::<_, ()>(ArithmeticInstructionSelect::RegisterToRegister(
                    crate::RegRegArithmetic { source, dest },
                ))
            },
        ),
        map_res(
            tuple((terminated(register, argument_sep), effective_address)),
            |(dest, source)| {
                Ok::<_, ()>(ArithmeticInstructionSelect::MemoryToRegister(
                    crate::MemRegArithmetic { source, dest },
                ))
            },
        ),
        map_res(
            tuple((terminated(effective_address, argument_sep), register)),
            |(dest, source)| {
                Ok::<_, ()>(ArithmeticInstructionSelect::RegisterToMemory(
                    crate::RegMemArithmetic { source, dest },
                ))
            },
        ),
        map_res(
            tuple((terminated(wide_register, argument_sep), literal_u8)),
            |(addr, literal)| {
                Ok::<_, ()>(ArithmeticInstructionSelect::ImmediateToRegisterByte(
                    addr, literal, true,
                ))
            },
        ),
        map_res(
            tuple((terminated(wide_register, argument_sep), literal_u16)),
            |(addr, literal)| {
                Ok::<_, ()>(ArithmeticInstructionSelect::ImmediateToRegisterWord(
                    addr, literal, false,
                ))
            },
        ),
        map_res(
            tuple((terminated(byte_register, argument_sep), literal_u8)),
            |(addr, literal)| {
                Ok::<_, ()>(ArithmeticInstructionSelect::ImmediateToRegisterByte(
                    addr, literal, false,
                ))
            },
        ),
        map_res(
            tuple((
                terminated(preceded(tag("word "), effective_address), argument_sep),
                literal_u8,
            )),
            |(addr, literal)| {
                Ok::<_, ()>(
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(
                        addr, literal, true,
                    ),
                )
            },
        ),
        map_res(
            tuple((
                terminated(preceded(tag("word "), effective_address), argument_sep),
                literal_u16,
            )),
            |(addr, literal)| {
                Ok::<_, ()>(
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(
                        addr, literal, false,
                    ),
                )
            },
        ),
        map_res(
            tuple((
                terminated(preceded(tag("byte "), effective_address), argument_sep),
                literal_u8,
            )),
            |(addr, literal)| {
                Ok::<_, ()>(
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(
                        addr, literal, false,
                    ),
                )
            },
        ),
        map_res(
            tuple((terminated(effective_address, argument_sep), literal_u16)),
            |(addr, literal)| {
                Ok::<_, ()>(
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(
                        addr, literal, false,
                    ),
                )
            },
        ),
        map_res(
            tuple((terminated(effective_address, argument_sep), literal_u8)),
            |(addr, literal)| {
                // TODO: where do we find this sign
                Ok::<_, ()>(
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(
                        addr, literal, false,
                    ),
                )
            },
        ),
    ))(input)
}

fn arithmetic_instruction(input: &str) -> IResult<&str, ArithmeticInstruction> {
    map_res(
        tuple((
            terminated(arithmetic_op, char(' ')),
            terminated(arithmetic_select, line_ending),
        )),
        |(op, instruction)| Ok::<_, ()>(ArithmeticInstruction { op, instruction }),
    )(input)
}

fn label(input: &str) -> IResult<&str, &str> {
    terminated(is_not(":"), terminated(char(':'), line_ending))(input)
}

fn jump(input: &str) -> IResult<&str, (Jump, &str)> {
    alt((
        map_res(preceded(tag("je "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Je, label))
        }),
        map_res(preceded(tag("jnz "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jne, label))
        }),
        map_res(preceded(tag("jnl "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jnl, label))
        }),
        map_res(preceded(tag("jb "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jb, label))
        }),
        map_res(preceded(tag("jnb "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jnb, label))
        }),
        map_res(preceded(tag("jbe "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jbe, label))
        }),
        map_res(preceded(tag("jl "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jl, label))
        }),
        map_res(preceded(tag("jg "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jnle, label))
        }),
        map_res(preceded(tag("jle "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jle, label))
        }),
        map_res(preceded(tag("jo "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jo, label))
        }),
        map_res(preceded(tag("jp "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jp, label))
        }),
        map_res(preceded(tag("js "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Js, label))
        }),
        map_res(preceded(tag("ja "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jnbe, label))
        }),
        map_res(preceded(tag("jnp "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jnp, label))
        }),
        map_res(preceded(tag("jno "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jno, label))
        }),
        map_res(preceded(tag("jns "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jns, label))
        }),
        map_res(preceded(tag("loop "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Loop, label))
        }),
        map_res(preceded(tag("loopz "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Loopz, label))
        }),
        map_res(preceded(tag("loopnz "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Loopnz, label))
        }),
        map_res(preceded(tag("jcxz "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jcxz, label))
        }),
        // a duplicate! check this
        map_res(preceded(tag("jne "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jne, label))
        }),
    ))(input)
}

fn instruction(input: &str) -> IResult<&str, Instruction<&str>> {
    alt((
        // This must come before MemRegMove.
        map_res(memory_to_accumulator_instruction, |v| {
            Ok::<_, ()>(Instruction::MemoryToAccumulator(v))
        }),
        // This must come before RegMemMove.
        map_res(accumulator_to_memory_instruction, |v| {
            Ok::<_, ()>(Instruction::AccumulatorToMemory(v))
        }),
        map_res(reg_reg_move_instruction, |v| {
            Ok::<_, ()>(Instruction::RegRegMove(v))
        }),
        map_res(reg_mem_move_instruction, |v| {
            Ok::<_, ()>(Instruction::RegMemMove(v))
        }),
        map_res(mem_reg_move_instruction, |v| {
            Ok::<_, ()>(Instruction::MemRegMove(v))
        }),
        map_res(immediate_to_register_instruction, |v| {
            Ok::<_, ()>(Instruction::ImmediateToRegister(v))
        }),
        map_res(immediate_to_memory_instruction, |v| {
            Ok::<_, ()>(Instruction::ImmediateToRegisterOrMemory(v))
        }),
        map_res(arithmetic_instruction, |v| {
            Ok::<_, ()>(Instruction::Arithmetic(v))
        }),
        map_res(jump, |(v, label)| Ok::<_, ()>(Instruction::Jump(v, label))),
        map_res(label, |v| {
            Ok::<_, ()>(Instruction::Trivia(TriviaInstruction::Label(v)))
        }),
    ))(input)
}

fn trivia(input: &str) -> IResult<&str, &str> {
    alt((comment, line_ending))(input)
}

pub fn program(input: &str) -> IResult<&str, Program<Vec<Instruction<&str>>, &str>> {
    map_res(
        preceded(
            many0(trivia),
            separated_pair(
                bits,
                many0(trivia),
                many0(alt((
                    map_res(instruction, |i| Ok::<_, ()>(Some(i))),
                    map_res(trivia, |_| Ok::<_, ()>(None)),
                ))),
            ),
        ),
        |(bits, instructions)| {
            Ok::<_, ()>(Program {
                bits,
                instructions: instructions.into_iter().flatten().collect(),
                offset: std::marker::PhantomData,
            })
        },
    )(input)
}
