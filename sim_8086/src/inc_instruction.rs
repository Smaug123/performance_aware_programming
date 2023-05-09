use crate::register::Register;
use arbitrary::Arbitrary;
use std::fmt::{Display, Formatter};

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub struct IncInstruction {
    pub target: Register,
    pub is_inc: bool,
}

impl Display for IncInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_inc {
            f.write_fmt(format_args!("inc {}", self.target))
        } else {
            f.write_fmt(format_args!("dec {}", self.target))
        }
    }
}

impl IncInstruction {
    pub fn to_bytes(&self) -> Vec<u8> {
        let (id, _is_wide) = self.target.to_id();
        vec![0b01000000 + id + if self.is_inc { 0 } else { 8 }]
    }

    pub fn length(&self) -> u8 {
        1
    }
}
