use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, line_ending, multispace0, not_line_ending, one_of},
    combinator::map_res,
    multi::many0,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::{
    register::{ByteRegisterSubset, GeneralRegister, Register, RegisterSubset, SpecialRegister},
    Base, EffectiveAddress, Instruction, MemRegMove, Program, RegMemMove, RegRegMove, SourceDest,
    WithOffset,
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

fn register(input: &str) -> IResult<&str, Register> {
    alt((
        map_res(tuple((general_register, register_subset)), |(reg, sub)| {
            Ok::<_, ()>(Register::General(reg, sub))
        }),
        map_res(special_register, |r| Ok::<_, ()>(Register::Special(r))),
    ))(input)
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
    preceded(
        char('['),
        terminated(map_res(digit1, str::parse), char(']')),
    )(input)
}

fn base(input: &str) -> IResult<&str, Base> {
    alt((
        map_res(tag("bx"), |_| Ok::<_, ()>(Base::Bx)),
        map_res(tag("bp"), |_| Ok::<_, ()>(Base::Bp)),
    ))(input)
}

fn source_dest(input: &str) -> IResult<&str, SourceDest> {
    alt((
        map_res(char('s'), |_| Ok::<_, ()>(SourceDest::Source)),
        map_res(char('d'), |_| Ok::<_, ()>(SourceDest::Dest)),
    ))(input)
}

fn absolute_u8(input: &str) -> IResult<&str, u8> {
    map_res(digit1, |digits| str::parse::<u8>(digits))(input)
}

fn absolute_u16(input: &str) -> IResult<&str, u16> {
    map_res(digit1, |digits| str::parse::<u16>(digits))(input)
}

fn effective_address(input: &str) -> IResult<&str, EffectiveAddress> {
    alt((
        // Sum, no offset
        map_res(
            tuple((base, ws(char('+')), source_dest)),
            |(base, _, source_dest)| {
                Ok::<_, ()>(EffectiveAddress::Sum(WithOffset::Basic((
                    base,
                    source_dest,
                ))))
            },
        ),
        // Sum, with positive offset
        map_res(
            tuple((base, ws(char('+')), source_dest, ws(char('+')), absolute_u8)),
            |(base, _, source_dest, _, offset)| {
                Ok::<_, ()>(EffectiveAddress::Sum(WithOffset::WithU8(
                    (base, source_dest),
                    offset,
                )))
            },
        ),
        map_res(
            tuple((
                base,
                ws(char('+')),
                source_dest,
                ws(char('+')),
                absolute_u16,
            )),
            |(base, _, source_dest, _, offset)| {
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
            tuple((source_dest, ws(char('+')), absolute_u8)),
            |(source_dest, _, offset)| {
                Ok::<_, ()>(EffectiveAddress::SpecifiedIn(WithOffset::WithU8(
                    source_dest,
                    offset,
                )))
            },
        ),
        map_res(
            tuple((source_dest, ws(char('+')), absolute_u16)),
            |(source_dest, _, offset)| {
                Ok::<_, ()>(EffectiveAddress::SpecifiedIn(WithOffset::WithU16(
                    source_dest,
                    offset,
                )))
            },
        ),
        // Offset from BX
        map_res(tag("bx"), |_| {
            Ok::<_, ()>(EffectiveAddress::Bx(WithOffset::Basic(())))
        }),
        map_res(
            tuple((tag("bx"), ws(char('+')), absolute_u8)),
            |(_, _, offset)| Ok::<_, ()>(EffectiveAddress::Bx(WithOffset::WithU8((), offset))),
        ),
        map_res(
            tuple((tag("bx"), ws(char('+')), absolute_u16)),
            |(_, _, offset)| Ok::<_, ()>(EffectiveAddress::Bx(WithOffset::WithU16((), offset))),
        ),
        // Direct memory address
        map_res(direct_offset, |offset| {
            Ok::<_, ()>(EffectiveAddress::Direct(offset))
        }),
        // Offset from base pointer
        map_res(
            tuple((tag("bp"), ws(char('+')), absolute_u8)),
            |(_, _, offset)| Ok::<_, ()>(EffectiveAddress::BasePointer(offset)),
        ),
        map_res(
            tuple((tag("bp"), ws(char('+')), absolute_u16)),
            |(_, _, offset)| Ok::<_, ()>(EffectiveAddress::BasePointerWide(offset)),
        ),
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

fn instruction(input: &str) -> IResult<&str, Instruction> {
    alt((
        map_res(reg_reg_move_instruction, |v| {
            Ok::<_, ()>(Instruction::RegRegMove(v))
        }),
        map_res(reg_mem_move_instruction, |v| {
            Ok::<_, ()>(Instruction::RegMemMove(v))
        }),
        map_res(mem_reg_move_instruction, |v| {
            Ok::<_, ()>(Instruction::MemRegMove(v))
        }),
    ))(input)
}

fn trivia(input: &str) -> IResult<&str, &str> {
    alt((comment, line_ending))(input)
}

pub fn program(input: &str) -> IResult<&str, Program<Vec<Instruction>>> {
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
            })
        },
    )(input)
}
