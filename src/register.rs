use std::fmt::{Display, Write};

#[derive(Eq, PartialEq)]
pub enum GeneralRegister {
    A,
    B,
    C,
    D,
}

impl GeneralRegister {
    pub fn to_id(&self) -> u8 {
        match self {
            GeneralRegister::A => 0b00,
            GeneralRegister::B => 0b11,
            GeneralRegister::C => 0b01,
            GeneralRegister::D => 0b10,
        }
    }
}

impl Display for GeneralRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GeneralRegister::A => f.write_char('a'),
            GeneralRegister::B => f.write_char('b'),
            GeneralRegister::C => f.write_char('c'),
            GeneralRegister::D => f.write_char('d'),
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum SpecialRegister {
    StackPointer,
    BasePointer,
    SourceIndex,
    DestIndex,
}

impl SpecialRegister {
    pub fn to_id(&self) -> u8 {
        // These are all wide.
        4 + match self {
            SpecialRegister::StackPointer => 0,
            SpecialRegister::BasePointer => 1,
            SpecialRegister::SourceIndex => 2,
            SpecialRegister::DestIndex => 3,
        }
    }
}

impl Display for SpecialRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpecialRegister::StackPointer => f.write_str("sp"),
            SpecialRegister::BasePointer => f.write_str("bp"),
            SpecialRegister::SourceIndex => f.write_str("si"),
            SpecialRegister::DestIndex => f.write_str("di"),
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum ByteRegisterSubset {
    High,
    Low,
}

#[derive(Eq, PartialEq)]
pub enum RegisterSubset {
    All,
    Subset(ByteRegisterSubset),
}

impl Display for RegisterSubset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterSubset::All => f.write_char('x'),
            RegisterSubset::Subset(ByteRegisterSubset::Low) => f.write_char('l'),
            RegisterSubset::Subset(ByteRegisterSubset::High) => f.write_char('h'),
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum Register {
    General(GeneralRegister, RegisterSubset),
    Special(SpecialRegister),
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::General(r, subset) => f.write_fmt(format_args!("{}{}", r, subset)),
            Register::Special(special) => f.write_fmt(format_args!("{}", special)),
        }
    }
}
