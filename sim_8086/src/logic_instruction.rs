use crate::effective_address::EffectiveAddress;
use crate::register::Register;
use arbitrary::Arbitrary;
use std::fmt::{Display, Formatter};

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum LogicInstructionType {
    Not,
    Shl,
    Shr,
    Sar,
    Rol,
    Ror,
    Rcl,
    Rcr,
}

impl Display for LogicInstructionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            LogicInstructionType::Not => "not",
            LogicInstructionType::Shl => "shl",
            LogicInstructionType::Shr => "shr",
            LogicInstructionType::Sar => "sar",
            LogicInstructionType::Rol => "rol",
            LogicInstructionType::Ror => "ror",
            LogicInstructionType::Rcl => "rcl",
            LogicInstructionType::Rcr => "rcr",
        })
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum LogicTarget {
    Address(EffectiveAddress),
    Register(Register),
}

impl Display for LogicTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicTarget::Address(addr) => f.write_fmt(format_args!("{}", addr)),
            LogicTarget::Register(reg) => f.write_fmt(format_args!("{}", reg)),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct LogicInstruction {
    pub op: LogicInstructionType,
    pub amount_from_cl: bool,
    pub target: LogicTarget,
    pub is_wide: bool,
}

impl Display for LogicInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{} {},{}",
            self.op,
            self.target,
            if self.amount_from_cl { "cl" } else { "1" }
        ))
    }
}

impl LogicInstruction {
    pub fn length(&self) -> u8 {
        match &self.target {
            LogicTarget::Register(_) => 2,
            LogicTarget::Address(addr) => 1 + addr.length(),
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut result = Vec::new();
        let byte = match self.op {
            LogicInstructionType::Not => 0b11110110,
            _ => 0b11010000 + if self.amount_from_cl { 2 } else { 0 },
        } + if self.is_wide { 1 } else { 0 };

        result.push(byte);

        let code = match &self.op {
            LogicInstructionType::Rol => 0,
            LogicInstructionType::Ror => 1,
            LogicInstructionType::Rcl => 2,
            LogicInstructionType::Rcr => 3,
            LogicInstructionType::Shl => 4,
            LogicInstructionType::Shr => 5,
            LogicInstructionType::Sar => 7,
            LogicInstructionType::Not => 2,
        };

        match &self.target {
            LogicTarget::Address(addr) => addr.push(code, &mut result),
            LogicTarget::Register(reg) => {
                let (id, _is_wide) = reg.to_id();
                result.push(3 * 64 + code * 8 + id)
            }
        };

        result
    }

    pub fn clock_count(&self) -> (u32, String) {
        match (&self.op, &self.target) {
            (LogicInstructionType::Not, LogicTarget::Register(_)) => (3, "".to_owned()),
            (LogicInstructionType::Not, LogicTarget::Address(addr)) => {
                let (count, result) = addr.clock_count();
                (count + 16, format!("16 {result}"))
            }
            (_, LogicTarget::Register(_)) => {
                if self.amount_from_cl {
                    todo!()
                } else {
                    (2, "".to_owned())
                }
            }
            (_, LogicTarget::Address(addr)) => {
                if self.amount_from_cl {
                    todo!()
                } else {
                    let (count, result) = addr.clock_count();
                    (count + 15, format!("15 {result}"))
                }
            }
        }
    }
}
