use const_panic::concat_panic;
use std::fmt::{Display, Write};

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum GeneralRegister {
    A,
    B,
    C,
    D,
}

impl GeneralRegister {
    pub const fn to_id(&self) -> u8 {
        match self {
            GeneralRegister::A => 0b00,
            GeneralRegister::B => 0b11,
            GeneralRegister::C => 0b01,
            GeneralRegister::D => 0b10,
        }
    }

    pub const fn of_id(id: u8) -> GeneralRegister {
        match id {
            0 => GeneralRegister::A,
            1 => GeneralRegister::C,
            2 => GeneralRegister::D,
            3 => GeneralRegister::B,
            _ => concat_panic!("Not a register: {}", id),
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

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum SpecialRegister {
    StackPointer,
    BasePointer,
    SourceIndex,
    DestIndex,
}

impl SpecialRegister {
    pub const fn to_id(&self) -> u8 {
        // These are all wide.
        4 + match self {
            SpecialRegister::StackPointer => 0,
            SpecialRegister::BasePointer => 1,
            SpecialRegister::SourceIndex => 2,
            SpecialRegister::DestIndex => 3,
        }
    }

    pub const fn of_id(id: u8) -> SpecialRegister {
        match id {
            4 => SpecialRegister::StackPointer,
            5 => SpecialRegister::BasePointer,
            6 => SpecialRegister::SourceIndex,
            7 => SpecialRegister::DestIndex,
            _ => concat_panic!("bad special register ID {}", id),
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

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum ByteRegisterSubset {
    High,
    Low,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
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

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
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

impl Register {
    pub const fn of_id(id: u8, is_wide: bool) -> Register {
        if is_wide {
            if id >= 4 {
                Register::Special(SpecialRegister::of_id(id))
            } else {
                Register::General(GeneralRegister::of_id(id), RegisterSubset::All)
            }
        } else {
            let subset = if id >= 4 {
                ByteRegisterSubset::High
            } else {
                ByteRegisterSubset::Low
            };
            Register::General(
                GeneralRegister::of_id(id % 4),
                RegisterSubset::Subset(subset),
            )
        }
    }

    // Returns true if the result is wide.
    pub const fn to_id(self: &Register) -> (u8, bool) {
        match self {
            Register::Special(s) => (s.to_id(), true),
            Register::General(reg, sub) => match sub {
                RegisterSubset::All => (reg.to_id(), true),
                RegisterSubset::Subset(ByteRegisterSubset::High) => (4 + reg.to_id(), false),
                RegisterSubset::Subset(ByteRegisterSubset::Low) => (reg.to_id(), false),
            },
        }
    }

    pub const fn is_wide(self: &Register) -> bool {
        match self {
            Register::Special(_) => true,
            Register::General(_, RegisterSubset::All) => true,
            Register::General(_, RegisterSubset::Subset(_)) => false,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
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

#[derive(Eq, PartialEq, Debug, Hash, Clone)]
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
