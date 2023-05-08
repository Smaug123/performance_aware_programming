use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{
        alphanumeric1, char, digit1, line_ending, multispace0, not_line_ending, one_of, space0,
    },
    combinator::{map_res, opt},
    error::FromExternalError,
    multi::{many0, many1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::boolean_instruction::{
    BooleanInstruction, BooleanInstructionDestination, BooleanInstructionType, ImmediateToReg,
    RegRegBoolean,
};
use crate::inc_instruction::IncInstruction;
use crate::{
    arithmetic_expression::{ArithmeticExpression, Token},
    arithmetic_instruction::{
        ArithmeticInstruction, ArithmeticInstructionSelect, ArithmeticOperation, MemRegArithmetic,
        RegMemArithmetic, RegRegArithmetic,
    },
    effective_address::{EffectiveAddress, WithOffset},
    instruction::Instruction,
    jump_instruction::Jump,
    move_instruction::{
        AccumulatorToMemory, ImmediateToMemory, ImmediateToRegister, MemRegMove,
        MemoryToAccumulator, MemoryToSegment, MoveInstruction, RegMemMove, RegRegMove,
        RegisterToSegment, SegmentToMemory, SegmentToRegister,
    },
    program::Program,
    register::{
        Base, ByteRegisterSubset, GeneralRegister, Register, RegisterSubset, SegmentRegister,
        SourceDest, SpecialRegister,
    },
    trivia_instruction::TriviaInstruction,
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

fn line_end(input: &str) -> IResult<&str, &str> {
    alt((comment, line_ending))(input)
}

#[derive(Eq, PartialEq)]
enum OffsetTag {
    Byte,
    Word,
    None,
}

fn bracketed<'a, F, O, E: nom::error::ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, (OffsetTag, O), E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
    E: FromExternalError<&'a str, ()>,
{
    map_res(
        tuple((
            opt(ws(alt((tag("byte"), tag("word"))))),
            preceded(char('['), terminated(inner, char(']'))),
        )),
        |(tag, inner)| {
            Ok::<_, _>(match tag {
                None => (OffsetTag::None, inner),
                Some(tag) => {
                    if tag == "byte" {
                        (OffsetTag::Byte, inner)
                    } else {
                        (OffsetTag::Word, inner)
                    }
                }
            })
        },
    )
}

fn argument_sep(input: &str) -> IResult<&str, ()> {
    map_res(ws(char(',')), |_| Ok::<_, ()>(()))(input)
}

fn bits(input: &str) -> IResult<&str, u8> {
    let p = preceded(tag("bits "), digit1);
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
            ws(tag("mov ")),
            tuple((terminated(register, argument_sep), register)),
        ),
        |(dest, source)| Ok::<_, ()>(RegRegMove { dest, source }),
    )(input)
}

// TODO: anything bracketed should be able to know if it's a byte or a word
fn direct_offset(input: &str) -> IResult<&str, (OffsetTag, u16)> {
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

fn literal_absolute_u8(input: &str) -> IResult<&str, u8> {
    alt((
        map_res(preceded(tag("0x"), alphanumeric1), |s: &str| {
            s.chars()
                .map(|x| {
                    if x.is_ascii_digit() {
                        Ok(x as u8 - b'0')
                    } else if x.is_ascii_hexdigit() {
                        Ok(x.to_ascii_lowercase() as u8 - b'a')
                    } else {
                        Err(())
                    }
                })
                .fold(Ok(0u8), |acc, new| match (acc, new) {
                    (Ok(acc), Ok(new)) => Ok(acc * 16 + new),
                    (_, Err(())) => Err(()),
                    (Err(()), _) => Err(()),
                })
        }),
        map_res(digit1, str::parse::<u8>),
        map_res(preceded(tag("byte "), digit1), str::parse::<u8>),
    ))(input)
}

fn literal_absolute_u16(input: &str) -> IResult<&str, u16> {
    alt((
        map_res(preceded(tag("0x"), alphanumeric1), |s: &str| {
            s.chars()
                .map(|x| {
                    if x.is_ascii_digit() {
                        Ok(x as u16 - '0' as u16)
                    } else if x.is_ascii_hexdigit() {
                        Ok(x.to_ascii_lowercase() as u16 - 'a' as u16)
                    } else {
                        Err(())
                    }
                })
                .fold(Ok(0u16), |acc, new| match (acc, new) {
                    (Ok(acc), Ok(new)) => Ok(acc * 16 + new),
                    (_, Err(())) => Err(()),
                    (Err(()), _) => Err(()),
                })
        }),
        map_res(digit1, str::parse::<u16>),
        map_res(preceded(tag("word "), digit1), str::parse::<u16>),
    ))(input)
}

fn arithmetic_expression_u8(input: &str) -> IResult<&str, ArithmeticExpression<u8>> {
    map_res(
        many1(alt((
            map_res(literal_absolute_u8, |x| Ok::<_, ()>(Token::Literal(x))),
            map_res(ws(char('+')), |_| Ok::<_, ()>(Token::Add)),
            map_res(ws(char('*')), |_| Ok::<_, ()>(Token::Times)),
        ))),
        |stream| Ok::<_, ()>(ArithmeticExpression::of_tokens(stream)),
    )(input)
}

fn arithmetic_expression_u16(input: &str) -> IResult<&str, ArithmeticExpression<u16>> {
    map_res(
        many1(alt((
            map_res(literal_absolute_u16, |x| Ok::<_, ()>(Token::Literal(x))),
            map_res(ws(char('+')), |_| Ok::<_, ()>(Token::Add)),
            map_res(ws(char('*')), |_| Ok::<_, ()>(Token::Times)),
        ))),
        |stream| Ok::<_, ()>(ArithmeticExpression::of_tokens(stream)),
    )(input)
}

fn absolute_u8(input: &str) -> IResult<&str, u8> {
    map_res(arithmetic_expression_u8, |expr| expr.eval())(input)
}

fn absolute_u16(input: &str) -> IResult<&str, u16> {
    map_res(arithmetic_expression_u16, |expr| expr.eval())(input)
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

fn literal_u16(input: &str) -> IResult<&str, u16> {
    alt((absolute_u16, negative_u16))(input)
}

fn literal_u8(input: &str) -> IResult<&str, u8> {
    alt((absolute_u8, negative_u8))(input)
}

fn effective_address(input: &str) -> IResult<&str, (OffsetTag, EffectiveAddress)> {
    alt((
        // Sum, no offset
        map_res(
            bracketed(tuple((base, ws(char('+')), source_dest))),
            |(tag, (base, _, source_dest))| {
                Ok::<_, ()>((
                    tag,
                    EffectiveAddress::Sum(WithOffset::Basic((base, source_dest))),
                ))
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
            |(tag, (base, _, source_dest, offset))| {
                Ok::<_, ()>((
                    tag,
                    EffectiveAddress::Sum(WithOffset::WithU8((base, source_dest), offset)),
                ))
            },
        ),
        map_res(
            bracketed(tuple((
                base,
                ws(char('+')),
                source_dest,
                alt((preceded(ws(char('+')), absolute_u16), negative_u16)),
            ))),
            |(tag, (base, _, source_dest, offset))| {
                Ok::<_, ()>((
                    tag,
                    EffectiveAddress::Sum(WithOffset::WithU16((base, source_dest), offset)),
                ))
            },
        ),
        // Lookup
        map_res(bracketed(source_dest), |(tag, source_dest)| {
            Ok::<_, ()>((
                tag,
                EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)),
            ))
        }),
        map_res(
            bracketed(tuple((
                source_dest,
                alt((preceded(ws(char('+')), absolute_u8), negative_u8)),
            ))),
            |(tag, (source_dest, offset))| {
                Ok::<_, ()>((
                    tag,
                    EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, offset)),
                ))
            },
        ),
        map_res(
            bracketed(tuple((
                source_dest,
                alt((preceded(ws(char('+')), absolute_u16), negative_u16)),
            ))),
            |(tag, (source_dest, offset))| {
                Ok::<_, ()>((
                    tag,
                    EffectiveAddress::SpecifiedIn(WithOffset::WithU16(source_dest, offset)),
                ))
            },
        ),
        // Offset from BX
        map_res(bracketed(tag("bx")), |(tag, _)| {
            Ok::<_, ()>((tag, EffectiveAddress::Bx(WithOffset::Basic(()))))
        }),
        map_res(
            bracketed(preceded(
                tag("bx"),
                alt((preceded(ws(char('+')), absolute_u8), negative_u8)),
            )),
            |(tag, offset)| {
                Ok::<_, ()>((tag, EffectiveAddress::Bx(WithOffset::WithU8((), offset))))
            },
        ),
        map_res(
            bracketed(preceded(
                tag("bx"),
                alt((preceded(ws(char('+')), absolute_u16), negative_u16)),
            )),
            |(tag, offset)| {
                Ok::<_, ()>((tag, EffectiveAddress::Bx(WithOffset::WithU16((), offset))))
            },
        ),
        // Direct memory address
        map_res(direct_offset, |(tag, offset)| {
            Ok::<_, ()>((tag, EffectiveAddress::Direct(offset)))
        }),
        // Offset from base pointer
        map_res(
            bracketed(preceded(
                tag("bp"),
                alt((preceded(ws(char('+')), absolute_u8), negative_u8)),
            )),
            |(tag, offset)| Ok::<_, ()>((tag, EffectiveAddress::BasePointer(offset))),
        ),
        map_res(
            bracketed(preceded(
                tag("bp"),
                alt((preceded(ws(char('+')), absolute_u16), negative_u16)),
            )),
            |(tag, offset)| Ok::<_, ()>((tag, EffectiveAddress::BasePointerWide(offset))),
        ),
        // Specific support for [bp], which can't be represented as a simple instruction
        map_res(bracketed(tag("bp")), |(tag, _)| {
            Ok::<_, ()>((tag, EffectiveAddress::BasePointer(0)))
        }),
    ))(input)
}

fn segment_register(input: &str) -> IResult<&str, SegmentRegister> {
    map_res(
        terminated(alt((char('s'), char('d'), char('e'), char('c'))), char('s')),
        |c| match c {
            's' => Ok::<_, ()>(SegmentRegister::Stack),
            'd' => Ok(SegmentRegister::Data),
            'e' => Ok(SegmentRegister::Extra),
            'c' => Ok(SegmentRegister::Code),
            _ => unreachable!(),
        },
    )(input)
}

fn reg_to_seg_move_instruction(input: &str) -> IResult<&str, RegisterToSegment> {
    map_res(
        preceded(
            ws(tag("mov ")),
            tuple((terminated(segment_register, ws(char(','))), register)),
        ),
        |(dest, source)| Ok::<_, ()>(RegisterToSegment { dest, source }),
    )(input)
}

fn mem_to_seg_move_instruction(input: &str) -> IResult<&str, MemoryToSegment> {
    map_res(
        preceded(
            ws(tag("mov ")),
            tuple((
                terminated(segment_register, ws(char(','))),
                effective_address,
            )),
        ),
        |(dest, (tag, source))| match tag {
            OffsetTag::Byte => Err(()),
            _ => Ok::<_, ()>(MemoryToSegment { dest, source }),
        },
    )(input)
}

fn seg_to_mem_move_instruction(input: &str) -> IResult<&str, SegmentToMemory> {
    map_res(
        preceded(
            ws(tag("mov ")),
            tuple((
                terminated(effective_address, ws(char(','))),
                segment_register,
            )),
        ),
        |((tag, dest), source)| match tag {
            OffsetTag::Byte => Err(()),
            _ => Ok::<_, ()>(SegmentToMemory { dest, source }),
        },
    )(input)
}

fn seg_to_reg_move_instruction(input: &str) -> IResult<&str, SegmentToRegister> {
    map_res(
        preceded(
            ws(tag("mov ")),
            tuple((terminated(register, ws(char(','))), segment_register)),
        ),
        |(dest, source)| Ok::<_, ()>(SegmentToRegister { dest, source }),
    )(input)
}

fn reg_mem_move_instruction(input: &str) -> IResult<&str, RegMemMove> {
    map_res(
        preceded(
            ws(tag("mov ")),
            tuple((terminated(effective_address, argument_sep), register)),
        ),
        |((tag, address), register)| match (tag, register.is_wide()) {
            (OffsetTag::Word, false) => Err(()),
            _ => Ok::<_, ()>(RegMemMove {
                dest: address,
                source: register,
            }),
        },
    )(input)
}

fn mem_reg_move_instruction(input: &str) -> IResult<&str, MemRegMove> {
    map_res(
        preceded(
            ws(tag("mov ")),
            tuple((terminated(register, argument_sep), effective_address)),
        ),
        |(register, (tag, address))| match (tag, register.is_wide()) {
            (OffsetTag::Word, false) => Err(()),
            _ => Ok::<_, ()>(MemRegMove {
                dest: register,
                source: address,
            }),
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
            ws(tag("mov ")),
            alt((
                map_res(immediate_wide, |(register, x)| {
                    Ok::<_, ()>((register, Err(x)))
                }),
                map_res(immediate_byte, |(register, x)| {
                    Ok::<_, ()>((register, Ok(x)))
                }),
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

fn immediate_to_memory_instruction(input: &str) -> IResult<&str, ImmediateToMemory> {
    map_res(
        tuple((
            terminated(preceded(ws(tag("mov ")), effective_address), argument_sep),
            alt((
                map_res(literal_u8, |x| Ok::<_, ()>(Ok(x))),
                map_res(literal_u16, |x| Ok::<_, ()>(Err(x))),
            )),
        )),
        |((tag, addr), x)| match tag {
            OffsetTag::None => Ok::<_, ()>(match x {
                Ok(b) => ImmediateToMemory::Byte(addr, b),
                Err(b) => ImmediateToMemory::Word(addr, b),
            }),
            OffsetTag::Byte => match x {
                Ok(b) => Ok(ImmediateToMemory::Byte(addr, b)),
                Err(b) => panic!("Can't fit literal {b} into byte memory"),
            },
            OffsetTag::Word => match x {
                Ok(b) => Ok(ImmediateToMemory::Word(addr, b as u16)),
                Err(b) => Ok(ImmediateToMemory::Word(addr, b)),
            },
        },
    )(input)
}

fn memory_to_accumulator_instruction(input: &str) -> IResult<&str, MemoryToAccumulator> {
    map_res(
        preceded(
            preceded(multispace0, tag("mov a")),
            tuple((
                terminated(alt((char('h'), char('x'))), argument_sep),
                bracketed(literal_u16),
            )),
        ),
        |(acc, (tag, address))| {
            let is_wide = acc == 'x';
            if !is_wide && tag == OffsetTag::Word {
                Err(())
            } else {
                Ok(MemoryToAccumulator { address, is_wide })
            }
        },
    )(input)
}

fn accumulator_to_memory_instruction(input: &str) -> IResult<&str, AccumulatorToMemory> {
    map_res(
        preceded(
            ws(tag("mov ")),
            tuple((
                terminated(bracketed(literal_u16), tuple((argument_sep, char('a')))),
                alt((char('h'), char('x'))),
            )),
        ),
        |((tag, address), acc)| {
            let is_wide = acc == 'x';
            if is_wide && tag == OffsetTag::Byte {
                Err(())
            } else {
                Ok(AccumulatorToMemory { address, is_wide })
            }
        },
    )(input)
}

fn boolean_op(input: &str) -> IResult<&str, BooleanInstructionType> {
    alt((
        map_res(tag("test"), |_| Ok::<_, ()>(BooleanInstructionType::Test)),
        map_res(tag("and"), |_| Ok::<_, ()>(BooleanInstructionType::And)),
        map_res(tag("or"), |_| Ok::<_, ()>(BooleanInstructionType::Or)),
        map_res(tag("xor"), |_| Ok::<_, ()>(BooleanInstructionType::Xor)),
    ))(input)
}

fn boolean_select(input: &str) -> IResult<&str, BooleanInstructionDestination> {
    alt((
        map_res(
            tuple((terminated(wide_register, argument_sep), wide_register)),
            |(dest, source)| {
                Ok::<_, ()>(BooleanInstructionDestination::RegReg(RegRegBoolean {
                    dest,
                    source,
                }))
            },
        ),
        map_res(
            tuple((terminated(wide_register, argument_sep), literal_u16)),
            |(dest, immediate)| {
                Ok::<_, ()>(BooleanInstructionDestination::ImmediateToReg(
                    ImmediateToReg::Wide(dest, immediate),
                ))
            },
        ),
        map_res(
            tuple((terminated(byte_register, argument_sep), literal_u8)),
            |(dest, immediate)| {
                Ok::<_, ()>(BooleanInstructionDestination::ImmediateToReg(
                    ImmediateToReg::Narrow(dest, immediate),
                ))
            },
        ),
    ))(input)
}

fn boolean_instruction(input: &str) -> IResult<&str, BooleanInstruction> {
    map_res(
        tuple((terminated(boolean_op, char(' ')), ws(boolean_select))),
        |(selection, dest)| Ok::<_, ()>(BooleanInstruction { dest, selection }),
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
                    RegRegArithmetic { source, dest },
                ))
            },
        ),
        map_res(
            tuple((terminated(register, argument_sep), effective_address)),
            |(dest, (tag, source))| match (tag, dest.is_wide()) {
                (OffsetTag::Word, false) => Err(()),
                _ => Ok(ArithmeticInstructionSelect::MemoryToRegister(
                    MemRegArithmetic { source, dest },
                )),
            },
        ),
        map_res(
            tuple((terminated(effective_address, argument_sep), register)),
            |((tag, dest), source)| match (tag, source.is_wide()) {
                (OffsetTag::Byte, true) => Err(()),
                _ => Ok(ArithmeticInstructionSelect::RegisterToMemory(
                    RegMemArithmetic { source, dest },
                )),
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
            tuple((terminated(effective_address, argument_sep), literal_u8)),
            |((tag, addr), literal)| {
                Ok::<_, ()>(
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(
                        addr,
                        literal,
                        tag == OffsetTag::Word,
                    ),
                )
            },
        ),
        map_res(
            tuple((terminated(effective_address, argument_sep), literal_u16)),
            |((tag, addr), literal)| match tag {
                OffsetTag::Byte => Err(()),
                _ => Ok::<_, ()>(
                    ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(addr, literal),
                ),
            },
        ),
    ))(input)
}

fn arithmetic_instruction(input: &str) -> IResult<&str, ArithmeticInstruction> {
    map_res(
        tuple((ws(terminated(arithmetic_op, char(' '))), arithmetic_select)),
        |(op, instruction)| Ok::<_, ()>(ArithmeticInstruction { op, instruction }),
    )(input)
}

fn label(input: &str) -> IResult<&str, &str> {
    terminated(is_not(":\r\n \t"), char(':'))(input)
}

fn label_terminator(input: &str) -> IResult<&str, &str> {
    is_not("\r\n;")(input)
}

fn jump(input: &str) -> IResult<&str, (Jump, &str)> {
    alt((
        map_res(preceded(tag("je "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Je, label))
        }),
        map_res(preceded(tag("jnz "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jne, label))
        }),
        map_res(preceded(tag("jnl "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jnl, label))
        }),
        map_res(preceded(tag("jb "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jb, label))
        }),
        map_res(preceded(tag("jnb "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jnb, label))
        }),
        map_res(preceded(tag("jbe "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jbe, label))
        }),
        map_res(preceded(tag("jl "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jl, label))
        }),
        map_res(preceded(tag("jg "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jnle, label))
        }),
        map_res(preceded(tag("jle "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jle, label))
        }),
        map_res(preceded(tag("jo "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jo, label))
        }),
        map_res(preceded(tag("jp "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jp, label))
        }),
        map_res(preceded(tag("js "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Js, label))
        }),
        map_res(preceded(tag("ja "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jnbe, label))
        }),
        map_res(preceded(tag("jnp "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jnp, label))
        }),
        map_res(preceded(tag("jno "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jno, label))
        }),
        map_res(preceded(tag("jns "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jns, label))
        }),
        map_res(preceded(tag("loop "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Loop, label))
        }),
        map_res(preceded(tag("loopz "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Loopz, label))
        }),
        map_res(preceded(tag("loopnz "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Loopnz, label))
        }),
        map_res(preceded(tag("jcxz "), label_terminator), |label| {
            Ok::<_, ()>((Jump::Jcxz, label))
        }),
        // a duplicate! check this
        map_res(preceded(tag("jne "), is_not("\n")), |label| {
            Ok::<_, ()>((Jump::Jne, label))
        }),
    ))(input)
}

fn move_instruction(input: &str) -> IResult<&str, MoveInstruction> {
    alt((
        // This must come before MemRegMove.
        map_res(memory_to_accumulator_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::MemoryToAccumulator(v))
        }),
        // This must come before RegMemMove.
        map_res(accumulator_to_memory_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::AccumulatorToMemory(v))
        }),
        map_res(reg_reg_move_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::RegRegMove(v))
        }),
        map_res(reg_mem_move_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::RegMemMove(v))
        }),
        map_res(mem_reg_move_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::MemRegMove(v))
        }),
        map_res(immediate_to_register_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::ImmediateToRegister(v))
        }),
        map_res(immediate_to_memory_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::ImmediateToMemory(v))
        }),
        map_res(seg_to_mem_move_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::SegmentToMemory(v))
        }),
        map_res(seg_to_reg_move_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::SegmentToRegister(v))
        }),
        map_res(reg_to_seg_move_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::RegisterToSegment(v))
        }),
        map_res(mem_to_seg_move_instruction, |v| {
            Ok::<_, ()>(MoveInstruction::MemoryToSegment(v))
        }),
    ))(input)
}

fn inc_instruction(input: &str) -> IResult<&str, IncInstruction> {
    preceded(
        ws(tag("inc ")),
        alt((
            map_res(effective_address, |(_, addr)| {
                Ok::<_, ()>(IncInstruction::Memory(addr))
            }),
            map_res(register, |reg| Ok::<_, ()>(IncInstruction::Register(reg))),
        )),
    )(input)
}

fn instruction(input: &str) -> IResult<&str, Instruction<&str>> {
    alt((
        map_res(move_instruction, |v| Ok::<_, ()>(Instruction::Move(v))),
        map_res(boolean_instruction, |v| {
            Ok::<_, ()>(Instruction::Boolean(v))
        }),
        map_res(arithmetic_instruction, |v| {
            Ok::<_, ()>(Instruction::Arithmetic(v))
        }),
        map_res(inc_instruction, |v| Ok::<_, ()>(Instruction::Inc(v))),
        map_res(tag("ret"), |_| Ok::<_, ()>(Instruction::Ret)),
        map_res(jump, |(v, label)| Ok::<_, ()>(Instruction::Jump(v, label))),
        map_res(label, |v| {
            Ok::<_, ()>(Instruction::Trivia(TriviaInstruction::Label(v)))
        }),
    ))(input)
}

pub fn program(input: &str) -> IResult<&str, Program<Vec<Instruction<&str>>, &str>> {
    map_res(
        preceded(
            many0(line_end),
            separated_pair(
                bits,
                many1(line_end),
                many0(alt((
                    map_res(preceded(space0, instruction), |i| Ok::<_, ()>(Some(i))),
                    map_res(preceded(space0, line_end), |_| Ok::<_, ()>(None)),
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

#[cfg(test)]
mod test_assembly {
    use crate::assembly::program;
    use crate::boolean_instruction::{
        BooleanInstruction, BooleanInstructionDestination, BooleanInstructionType, RegRegBoolean,
    };
    use crate::register::SpecialRegister;
    use crate::{
        arithmetic_instruction::{
            ArithmeticInstruction, ArithmeticInstructionSelect, ArithmeticOperation,
        },
        assembly::instruction,
        effective_address::{EffectiveAddress, WithOffset},
        instruction::Instruction,
        move_instruction::{ImmediateToMemory, MemoryToAccumulator, MoveInstruction},
        register::{GeneralRegister, Register, RegisterSubset},
    };

    #[test]
    fn arithmetic_expression_parse() {
        let (remaining, parsed) = instruction("add bx, 4*64").unwrap();
        assert_eq!(remaining, "");
        assert_eq!(
            parsed,
            Instruction::Arithmetic(ArithmeticInstruction {
                op: ArithmeticOperation::Add,
                instruction: ArithmeticInstructionSelect::ImmediateToRegisterWord(
                    Register::General(GeneralRegister::B, RegisterSubset::All),
                    4 * 64,
                    false
                )
            })
        )
    }

    #[test]
    fn arithmetic_expression_parse_2() {
        let (remaining, parsed) = instruction("add cx, 166").unwrap();
        assert_eq!(remaining, "");
        assert_eq!(
            parsed,
            Instruction::Arithmetic(ArithmeticInstruction {
                op: ArithmeticOperation::Add,
                instruction: ArithmeticInstructionSelect::ImmediateToRegisterByte(
                    Register::General(GeneralRegister::C, RegisterSubset::All),
                    166,
                    true
                )
            })
        )
    }

    #[test]
    fn mov_acc_parse() {
        let (remaining, parsed) = instruction("mov ax, [16]").unwrap();
        assert_eq!(remaining, "");
        assert_eq!(
            parsed,
            Instruction::Move(MoveInstruction::MemoryToAccumulator(MemoryToAccumulator {
                address: 16,
                is_wide: true
            }))
        )
    }

    #[test]
    fn mov_acc_parse_program() {
        let (remaining, parsed) = program("bits 16\nmov ax, [2555]\nmov ax, [16]").unwrap();
        assert_eq!(remaining, "");
        assert_eq!(parsed.bits, 16);
        let parsed = parsed.instructions;
        assert_eq!(parsed.len(), 2);
        assert_eq!(
            parsed[0].clone(),
            Instruction::Move(MoveInstruction::MemoryToAccumulator(MemoryToAccumulator {
                address: 2555,
                is_wide: true
            }))
        );
        assert_eq!(
            parsed[1].clone(),
            Instruction::Move(MoveInstruction::MemoryToAccumulator(MemoryToAccumulator {
                address: 16,
                is_wide: true
            }))
        );
    }

    #[test]
    fn mov_parse() {
        let (remaining, parsed) = instruction("mov byte [bx + 61*4 + 1], 255").unwrap();
        assert_eq!(remaining, "");
        assert_eq!(
            parsed,
            Instruction::Move(MoveInstruction::ImmediateToMemory(ImmediateToMemory::Byte(
                EffectiveAddress::Bx(WithOffset::WithU8((), 61 * 4 + 1)),
                255
            )))
        )
    }

    #[test]
    fn add_immediate_neg_wide() {
        let (remaining, parsed) = instruction("add cx, -90").unwrap();
        assert_eq!(remaining, "");
        assert_eq!(
            parsed,
            Instruction::Arithmetic(ArithmeticInstruction {
                op: ArithmeticOperation::Add,
                instruction: ArithmeticInstructionSelect::ImmediateToRegisterByte(
                    Register::General(GeneralRegister::C, RegisterSubset::All),
                    u8::MAX - 90 + 1,
                    true
                )
            })
        )
    }

    #[test]
    fn test_test_reg_reg() {
        let parsed = program("bits 16\ntest    di, di\r\n");
        let (remaining, parsed) = parsed.unwrap();
        assert_eq!(remaining, "");
        let parsed = parsed.instructions;
        assert_eq!(parsed.len(), 1);
        let parsed = &parsed[0];
        assert_eq!(
            parsed.clone(),
            Instruction::Boolean(BooleanInstruction {
                selection: BooleanInstructionType::Test,
                dest: BooleanInstructionDestination::RegReg(RegRegBoolean {
                    dest: Register::Special(SpecialRegister::DestIndex),
                    source: Register::Special(SpecialRegister::DestIndex),
                }),
            })
        )
    }
}
