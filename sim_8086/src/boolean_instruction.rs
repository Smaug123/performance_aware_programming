use crate::effective_address::EffectiveAddress;
use crate::register::Register;
use arbitrary::Arbitrary;
use std::fmt::{Display, Formatter};

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum BooleanInstructionType {
    Test,
    And,
    Or,
    Xor,
}

impl Display for BooleanInstructionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            BooleanInstructionType::Test => "test",
            BooleanInstructionType::And => "and",
            BooleanInstructionType::Or => "or",
            BooleanInstructionType::Xor => "xor",
        })
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct RegRegBoolean {
    pub dest: Register,
    pub source: Register,
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum ImmediateToMem {
    Wide(EffectiveAddress, u16),
    Narrow(EffectiveAddress, u8),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum ImmediateToAcc {
    Wide(u16),
    Narrow(u8),
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum BooleanInstructionDestination {
    RegReg(RegRegBoolean),
    ImmediateToAcc(ImmediateToAcc),
}

impl Display for BooleanInstructionDestination {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BooleanInstructionDestination::RegReg(inst) => {
                f.write_fmt(format_args!("{}, {}", inst.dest, inst.source))
            }
            BooleanInstructionDestination::ImmediateToAcc(inst) => match inst {
                ImmediateToAcc::Wide(wide) => f.write_fmt(format_args!("ax, {}", wide)),
                ImmediateToAcc::Narrow(narrow) => f.write_fmt(format_args!("al, {}", narrow)),
            },
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct BooleanInstruction {
    pub selection: BooleanInstructionType,
    pub dest: BooleanInstructionDestination,
}

impl Display for BooleanInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} {}", self.selection, self.dest))
    }
}

impl BooleanInstruction {
    pub fn to_bytes(&self) -> Vec<u8> {
        match &self.dest {
            /*
            BooleanInstructionDestination::ImmediateToMem(immediate) => {
                let mut result = Vec::with_capacity(2);
                let (opcode, reg) = match self.selection {
                    BooleanInstructionType::Test => (0b11110111, 0),
                    BooleanInstructionType::And => (0b10000000, 0b100),
                    BooleanInstructionType::Or => (0b10000000, 0b001),
                    BooleanInstructionType::Xor => {
                        todo!()
                        // (0b00110100, todo!())
                    }
                };

                match immediate {
                    ImmediateToMem::Wide(dest, data) => {
                        result.push(opcode + 1);
                        dest.push(reg, &mut result);
                        result.push((data % 256) as u8);
                        result.push((data / 256) as u8);
                    }
                    ImmediateToMem::Narrow(dest, data) => {
                        result.push(opcode);
                        dest.push(reg, &mut result);
                        result.push(*data);
                    }
                }

                result
            }
             */
            BooleanInstructionDestination::RegReg(reg_reg) => {
                let mut result = Vec::with_capacity(2);
                let opcode = match self.selection {
                    BooleanInstructionType::And => 0b00100000,
                    BooleanInstructionType::Test => 0b10000100,
                    BooleanInstructionType::Or => 0b00001000,
                    BooleanInstructionType::Xor => 0b00110000,
                };

                let (reg, is_wide) = reg_reg.dest.to_id();
                let mode = 3;
                let (rm, is_wide_2) = reg_reg.source.to_id();
                if is_wide != is_wide_2 {
                    panic!("conflicting wideness")
                }
                if is_wide {
                    result.push(opcode + 1);
                } else {
                    result.push(opcode);
                }
                result.push(mode * 64 + reg * 8 + rm);

                result
            }
            BooleanInstructionDestination::ImmediateToAcc(data) => {
                let mut result = Vec::with_capacity(2);
                let opcode = match self.selection {
                    BooleanInstructionType::Test => 0b10101000,
                    BooleanInstructionType::And => 0b00100100,
                    BooleanInstructionType::Or => 0b00001100,
                    BooleanInstructionType::Xor => 0b00110100,
                };
                match data {
                    ImmediateToAcc::Wide(data) => {
                        result.push(opcode + 1);
                        result.push((data % 256) as u8);
                        result.push((data / 256) as u8);
                    }
                    ImmediateToAcc::Narrow(data) => {
                        result.push(opcode);
                        result.push(*data);
                    }
                }

                result
            }
        }
    }

    pub fn length(&self) -> u8 {
        match &self.dest {
            BooleanInstructionDestination::ImmediateToAcc(data) => match data {
                ImmediateToAcc::Wide(_) => 3,
                ImmediateToAcc::Narrow(_) => 2,
            },
            BooleanInstructionDestination::RegReg(_) => 2,
        }
    }

    pub fn clock_count(&self) -> (u32, String) {
        match self.selection {
            BooleanInstructionType::Test => match self.dest {
                BooleanInstructionDestination::ImmediateToAcc(_) => (4, "".to_owned()),
                BooleanInstructionDestination::RegReg(_) => (3, "".to_owned()),
            },
            BooleanInstructionType::And => match self.dest {
                BooleanInstructionDestination::ImmediateToAcc(_) => (4, "".to_owned()),
                BooleanInstructionDestination::RegReg(_) => (3, "".to_owned()),
            },
            BooleanInstructionType::Or => match self.dest {
                BooleanInstructionDestination::ImmediateToAcc(_) => (4, "".to_owned()),
                BooleanInstructionDestination::RegReg(_) => (3, "".to_owned()),
            },
            BooleanInstructionType::Xor => match self.dest {
                BooleanInstructionDestination::ImmediateToAcc(_) => (4, "".to_owned()),
                BooleanInstructionDestination::RegReg(_) => (3, "".to_owned()),
            },
        }
    }
}
