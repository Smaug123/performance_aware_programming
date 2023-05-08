use crate::register::Register;
use arbitrary::Arbitrary;
use std::fmt::{Display, Formatter};

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum IncInstruction {
    Register(Register),
}

impl Display for IncInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IncInstruction::Register(reg) => f.write_fmt(format_args!("inc {}", reg)),
        }
    }
}

impl IncInstruction {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            IncInstruction::Register(reg) => {
                let (id, _is_wide) = reg.to_id();
                vec![0b01000000 + id]
            }
        }
    }

    pub fn length(&self) -> u8 {
        match self {
            IncInstruction::Register(_) => 1,
        }
    }
}
