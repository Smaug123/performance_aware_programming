use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, line_ending, not_line_ending, one_of},
    combinator::map_res,
    multi::many0,
    sequence::{preceded, separated_pair, terminated, tuple},
    IResult,
};

use crate::{
    register::{ByteRegisterSubset, GeneralRegister, Register, RegisterSubset, SpecialRegister},
    Instruction, MoveInstruction, Program,
};

fn comment(input: &str) -> IResult<&str, &str> {
    preceded(char(';'), terminated(not_line_ending, line_ending))(input)
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

fn move_instruction(input: &str) -> IResult<&str, MoveInstruction> {
    map_res(
        preceded(
            tag("mov "),
            tuple((register, tag(", "), register, line_ending)),
        ),
        |(dest, _, source, _)| Ok::<_, ()>(MoveInstruction { dest, source }),
    )(input)
}

fn instruction(input: &str) -> IResult<&str, Instruction> {
    map_res(move_instruction, |v| Ok::<_, ()>(Instruction::Move(v)))(input)
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
