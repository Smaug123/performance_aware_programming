use std::fmt::Display;

use arbitrary::Arbitrary;

#[derive(Eq, PartialEq, Debug, Hash, Clone, Copy, Arbitrary)]
pub enum Jump {
    Je,
    Jl,
    Jle,
    Jb,
    Jbe,
    Jp,
    Jo,
    Js,
    Jne,
    Jnl,
    Jnle,
    Jnb,
    Jnbe,
    Jnp,
    Jno,
    Jns,
    Loop,
    Loopz,
    Loopnz,
    Jcxz,
}

impl Display for Jump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Jump::Je => "je",
            Jump::Jl => "jl",
            Jump::Jle => "jle",
            Jump::Jb => "jb",
            Jump::Jbe => "jbe",
            Jump::Jp => "jp",
            Jump::Jo => "jo",
            Jump::Js => "js",
            Jump::Jne => "jne",
            Jump::Jnl => "jnl",
            Jump::Jnle => "jnle",
            Jump::Jnb => "jnb",
            Jump::Jnbe => "jnbe",
            Jump::Jnp => "jnp",
            Jump::Jno => "jno",
            Jump::Jns => "jns",
            Jump::Loop => "loop",
            Jump::Loopz => "loopz",
            Jump::Loopnz => "loopnz",
            Jump::Jcxz => "jcxz",
        })
    }
}

impl Jump {
    #[must_use]
    pub fn clock_count(&self, is_success: bool) -> u32 {
        match self {
            Jump::Jle
            | Jump::Jp
            | Jump::Jo
            | Jump::Js
            | Jump::Jne
            | Jump::Jl
            | Jump::Jnl
            | Jump::Jnle
            | Jump::Jnb
            | Jump::Jb
            | Jump::Jbe
            | Jump::Je
            | Jump::Jnp
            | Jump::Jno
            | Jump::Jns
            | Jump::Jnbe => {
                if is_success {
                    4
                } else {
                    16
                }
            }
            Jump::Loop => {
                if is_success {
                    5
                } else {
                    17
                }
            }
            Jump::Loopnz => {
                if is_success {
                    5
                } else {
                    19
                }
            }
            Jump::Loopz | Jump::Jcxz => {
                if is_success {
                    6
                } else {
                    18
                }
            }
        }
    }
}
