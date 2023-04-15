use std::fmt::Display;

use arbitrary::Arbitrary;

use crate::register::{Base, SourceDest};

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum WithOffset<T> {
    Basic(T),
    WithU8(T, u8),
    WithU16(T, u16),
}

impl<T> Display for WithOffset<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WithOffset::Basic(t) => f.write_fmt(format_args!("{}", t)),
            WithOffset::WithU8(t, offset) => f.write_fmt(format_args!("[{} + {}]", t, offset)),
            WithOffset::WithU16(t, offset) => f.write_fmt(format_args!("[{} + {}]", t, offset)),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum EffectiveAddress {
    /// An offset from the contents of the (BX/BP) register plus the contents of the (SI/DI) register.
    Sum(WithOffset<(Base, SourceDest)>),
    /// An offset from the contents of the SI or DI register.
    SpecifiedIn(WithOffset<SourceDest>),
    /// An offset from the contents of the BX register.
    Bx(WithOffset<()>),
    /// A literal index into memory.
    Direct(u16),
    /// An offset from the contents of the BP register.
    BasePointer(u8),
    /// An offset from the contents of the BP register.
    BasePointerWide(u16),
}

impl Display for EffectiveAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EffectiveAddress::Sum(w) => match w {
                WithOffset::Basic((base, source_dest)) => {
                    f.write_fmt(format_args!("[{} + {}i]", base, source_dest))
                }
                WithOffset::WithU8((base, source_dest), offset) => {
                    f.write_fmt(format_args!("[{} + {}i + {}]", base, source_dest, offset))
                }
                WithOffset::WithU16((base, source_dest), offset) => {
                    f.write_fmt(format_args!("[{} + {}i + {}]", base, source_dest, offset))
                }
            },
            EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)) => {
                f.write_fmt(format_args!("[{}i]", source_dest))
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, offset)) => {
                f.write_fmt(format_args!("[{}i + {}]", source_dest, offset))
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU16(source_dest, offset)) => {
                f.write_fmt(format_args!("[{}i + {}]", source_dest, offset))
            }
            EffectiveAddress::Bx(offset) => match offset {
                WithOffset::Basic(()) => f.write_str("bx"),
                WithOffset::WithU8((), offset) => f.write_fmt(format_args!("[bx + {}]", offset)),
                WithOffset::WithU16((), offset) => f.write_fmt(format_args!("[bx + {}]", offset)),
            },
            EffectiveAddress::Direct(location) => f.write_fmt(format_args!("[{}]", location)),
            EffectiveAddress::BasePointer(offset) => f.write_fmt(format_args!("[bp + {}]", offset)),
            EffectiveAddress::BasePointerWide(offset) => {
                f.write_fmt(format_args!("[bp + {}]", offset))
            }
        }
    }
}

impl EffectiveAddress {
    pub(crate) fn of_mode_rm<I>(mode: u8, rm: u8, bytes: &mut I) -> EffectiveAddress
    where
        I: Iterator<Item = u8>,
    {
        let source_dest = if rm % 2 == 0 {
            SourceDest::Source
        } else {
            SourceDest::Dest
        };
        let base = if (rm / 2) % 2 == 0 {
            Base::Bx
        } else {
            Base::Bp
        };
        let displacement_low = if rm == 6 || mode > 0 {
            bytes.next().expect("required an 8-bit displacement")
        } else {
            0
        };
        let displacement_high = if (rm == 6 && mode == 0) || mode == 2 {
            let high = bytes.next().expect("required a 16-bit displacement");
            (high as u16) * 256 + (displacement_low as u16)
        } else {
            0
        };

        if rm < 4 {
            match mode {
                0 => EffectiveAddress::Sum(WithOffset::Basic((base, source_dest))),
                1 => {
                    EffectiveAddress::Sum(WithOffset::WithU8((base, source_dest), displacement_low))
                }
                2 => EffectiveAddress::Sum(WithOffset::WithU16(
                    (base, source_dest),
                    displacement_high,
                )),
                _ => panic!("Got bad mode: {}", mode),
            }
        } else if rm < 6 {
            match mode {
                0 => EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)),
                1 => {
                    EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, displacement_low))
                }
                2 => EffectiveAddress::SpecifiedIn(WithOffset::WithU16(
                    source_dest,
                    displacement_high,
                )),
                _ => panic!("Got bad mode: {}", mode),
            }
        } else if rm == 6 {
            match mode {
                0 => EffectiveAddress::Direct(displacement_high),
                1 => EffectiveAddress::BasePointer(displacement_low),
                2 => EffectiveAddress::BasePointerWide(displacement_high),
                _ => panic!("Got bad mode: {}", mode),
            }
        } else {
            assert_eq!(rm, 7);
            match mode {
                0 => EffectiveAddress::Bx(WithOffset::Basic(())),
                1 => EffectiveAddress::Bx(WithOffset::WithU8((), displacement_low)),
                2 => EffectiveAddress::Bx(WithOffset::WithU16((), displacement_high)),
                _ => panic!("Got bad mode: {}", mode),
            }
        }
    }

    pub(crate) fn push(&self, reg: u8, result: &mut Vec<u8>) {
        match self {
            EffectiveAddress::Sum(WithOffset::Basic((base, source_dest))) => {
                let mode = 0u8;
                let rm = match base {
                    Base::Bx => 0u8,
                    Base::Bp => 2,
                } + match source_dest {
                    SourceDest::Source => 0,
                    SourceDest::Dest => 1,
                };
                result.push(mode * 64 + reg * 8 + rm);
            }
            EffectiveAddress::Sum(WithOffset::WithU8((base, source_dest), offset)) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = match base {
                    Base::Bx => 0u8,
                    Base::Bp => 2,
                } + match source_dest {
                    SourceDest::Source => 0,
                    SourceDest::Dest => 1,
                };
                result.push(mode * 64 + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::Sum(WithOffset::WithU16((base, source_dest), offset)) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = match base {
                    Base::Bx => 0u8,
                    Base::Bp => 2,
                } + match source_dest {
                    SourceDest::Source => 0,
                    SourceDest::Dest => 1,
                };
                result.push(mode * 64 + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)) => {
                let mode = 0u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode * 64 + reg * 8 + rm);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, offset)) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode * 64 + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU16(source_dest, offset)) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = match source_dest {
                    SourceDest::Source => 4u8,
                    SourceDest::Dest => 5,
                };
                result.push(mode * 64 + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::Bx(WithOffset::Basic(())) => {
                let mode = 0u8;
                let rm = 7u8;
                result.push(mode * 64 + reg * 8 + rm);
            }
            EffectiveAddress::Bx(WithOffset::WithU8((), offset)) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = 7u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::Bx(WithOffset::WithU16((), offset)) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = 7u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
            EffectiveAddress::Direct(address) => {
                result.reserve_exact(2);
                let mode = 0u8;
                let rm = 6u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push((address % 256) as u8);
                result.push((address / 256) as u8);
            }
            EffectiveAddress::BasePointer(offset) => {
                result.reserve_exact(1);
                let mode = 1u8;
                let rm = 6u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push(*offset);
            }
            EffectiveAddress::BasePointerWide(offset) => {
                result.reserve_exact(2);
                let mode = 2u8;
                let rm = 6u8;
                result.push(mode * 64 + reg * 8 + rm);
                result.push((offset % 256) as u8);
                result.push((offset / 256) as u8);
            }
        }
    }

    pub(crate) const fn length(&self) -> u8 {
        match self {
            EffectiveAddress::Sum(WithOffset::Basic(_)) => 1,
            EffectiveAddress::Sum(WithOffset::WithU8(_, _)) => 2,
            EffectiveAddress::Sum(WithOffset::WithU16(_, _)) => 3,
            EffectiveAddress::SpecifiedIn(WithOffset::Basic(_)) => 1,
            EffectiveAddress::SpecifiedIn(WithOffset::WithU8(_, _)) => 2,
            EffectiveAddress::SpecifiedIn(WithOffset::WithU16(_, _)) => 3,
            EffectiveAddress::Bx(WithOffset::Basic(())) => 1,
            EffectiveAddress::Bx(WithOffset::WithU8((), _)) => 2,
            EffectiveAddress::Bx(WithOffset::WithU16((), _)) => 3,
            EffectiveAddress::Direct(_) => 3,
            EffectiveAddress::BasePointer(_) => 2,
            EffectiveAddress::BasePointerWide(_) => 3,
        }
    }
}
