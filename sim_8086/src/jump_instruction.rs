use std::fmt::Display;

use arbitrary::Arbitrary;

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
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
