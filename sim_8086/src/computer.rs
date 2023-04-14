use crate::{
    effective_address::{EffectiveAddress, WithOffset},
    instruction::Instruction,
    move_instruction::{ImmediateToMemory, ImmediateToRegister, MoveInstruction},
    register::{
        Base, ByteRegisterSubset, GeneralRegister, Register, RegisterSubset, SegmentRegister,
        SourceDest, SpecialRegister,
    },
};

pub struct Registers {
    a: u16,
    b: u16,
    c: u16,
    d: u16,
    si: u16,
    di: u16,
    sp: u16,
    bp: u16,
    ss: u16,
    cs: u16,
    es: u16,
    ds: u16,
}

pub struct Computer {
    #[allow(dead_code)]
    memory: [u8; 65536],
    registers: Registers,
}

impl Computer {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Computer {
        Computer {
            memory: [0; 65536],
            registers: Registers {
                a: 0,
                b: 0,
                c: 0,
                d: 0,
                si: 0,
                di: 0,
                sp: 0,
                bp: 0,
                ss: 0,
                cs: 0,
                es: 0,
                ds: 0,
            },
        }
    }

    /// If the input register is General(_, Subset(_)), the resulting u16 is at most 255.
    /// That is, we'll return you the correct number as if it were a u8 that is cast to u16.
    fn get_register(&self, r: &Register) -> u16 {
        match r {
            Register::General(GeneralRegister::A, RegisterSubset::All) => self.registers.a,
            Register::General(GeneralRegister::B, RegisterSubset::All) => self.registers.b,
            Register::General(GeneralRegister::C, RegisterSubset::All) => self.registers.c,
            Register::General(GeneralRegister::D, RegisterSubset::All) => self.registers.d,
            Register::General(
                GeneralRegister::A,
                RegisterSubset::Subset(ByteRegisterSubset::Low),
            ) => self.registers.a % 256,
            Register::General(
                GeneralRegister::B,
                RegisterSubset::Subset(ByteRegisterSubset::Low),
            ) => self.registers.b % 256,
            Register::General(
                GeneralRegister::C,
                RegisterSubset::Subset(ByteRegisterSubset::Low),
            ) => self.registers.c % 256,
            Register::General(
                GeneralRegister::D,
                RegisterSubset::Subset(ByteRegisterSubset::Low),
            ) => self.registers.d % 256,
            Register::General(
                GeneralRegister::A,
                RegisterSubset::Subset(ByteRegisterSubset::High),
            ) => self.registers.a / 256,
            Register::General(
                GeneralRegister::B,
                RegisterSubset::Subset(ByteRegisterSubset::High),
            ) => self.registers.b / 256,
            Register::General(
                GeneralRegister::C,
                RegisterSubset::Subset(ByteRegisterSubset::High),
            ) => self.registers.c / 256,
            Register::General(
                GeneralRegister::D,
                RegisterSubset::Subset(ByteRegisterSubset::High),
            ) => self.registers.d / 256,
            Register::Special(SpecialRegister::BasePointer) => self.registers.bp,
            Register::Special(SpecialRegister::StackPointer) => self.registers.sp,
            Register::Special(SpecialRegister::SourceIndex) => self.registers.si,
            Register::Special(SpecialRegister::DestIndex) => self.registers.di,
        }
    }

    fn get_segment(&self, r: SegmentRegister) -> u16 {
        match r {
            SegmentRegister::Code => self.registers.cs,
            SegmentRegister::Data => self.registers.ds,
            SegmentRegister::Stack => self.registers.ss,
            SegmentRegister::Extra => self.registers.es,
        }
    }

    fn display_big(u: u16) -> String {
        if u == 0 {
            "0x0".to_owned()
        } else {
            format!("{:#06x}", u)
        }
    }

    fn display_small(u: u16) -> String {
        if u == 0 {
            "0x0".to_owned()
        } else {
            format!("{:#x}", u)
        }
    }

    fn set_register(&mut self, r: &Register, value: u16) -> String {
        let register_for_print = match r {
            Register::General(x, _) => Register::General(x.clone(), RegisterSubset::All),
            _ => r.clone(),
        };
        let was = self.get_register(&register_for_print);
        match r {
            Register::General(GeneralRegister::A, RegisterSubset::All) => self.registers.a = value,
            Register::General(GeneralRegister::B, RegisterSubset::All) => self.registers.b = value,
            Register::General(GeneralRegister::C, RegisterSubset::All) => self.registers.c = value,
            Register::General(GeneralRegister::D, RegisterSubset::All) => self.registers.d = value,
            Register::General(
                GeneralRegister::A,
                RegisterSubset::Subset(ByteRegisterSubset::Low),
            ) => {
                assert!(value <= 255);
                self.registers.a = self.registers.a - (self.registers.a % 256) + value
            }
            Register::General(
                GeneralRegister::B,
                RegisterSubset::Subset(ByteRegisterSubset::Low),
            ) => {
                assert!(value <= 255);
                self.registers.b = self.registers.b - (self.registers.b % 256) + value
            }
            Register::General(
                GeneralRegister::C,
                RegisterSubset::Subset(ByteRegisterSubset::Low),
            ) => {
                assert!(value <= 255);
                self.registers.c = self.registers.c - (self.registers.c % 256) + value
            }
            Register::General(
                GeneralRegister::D,
                RegisterSubset::Subset(ByteRegisterSubset::Low),
            ) => {
                assert!(value <= 255);
                self.registers.d = self.registers.d - (self.registers.d % 256) + value
            }
            Register::General(
                GeneralRegister::A,
                RegisterSubset::Subset(ByteRegisterSubset::High),
            ) => {
                assert!(value <= 255);
                self.registers.a = (self.registers.a % 256) + value * 256
            }
            Register::General(
                GeneralRegister::B,
                RegisterSubset::Subset(ByteRegisterSubset::High),
            ) => {
                assert!(value <= 255);
                self.registers.b = (self.registers.b % 256) + value * 256
            }
            Register::General(
                GeneralRegister::C,
                RegisterSubset::Subset(ByteRegisterSubset::High),
            ) => {
                assert!(value <= 255);
                self.registers.c = (self.registers.c % 256) + value * 256
            }
            Register::General(
                GeneralRegister::D,
                RegisterSubset::Subset(ByteRegisterSubset::High),
            ) => {
                assert!(value <= 255);
                self.registers.d = (self.registers.d % 256) + value * 256
            }
            Register::Special(SpecialRegister::BasePointer) => self.registers.bp = value,
            Register::Special(SpecialRegister::StackPointer) => self.registers.sp = value,
            Register::Special(SpecialRegister::SourceIndex) => self.registers.si = value,
            Register::Special(SpecialRegister::DestIndex) => self.registers.di = value,
        }
        let is_now = self.get_register(&register_for_print);
        // TODO: this needs to print out "al: 0x22 -> blah" instead of "ax: 0x2222 -> blah" if short
        format!(
            "{}:{}->{}",
            register_for_print,
            Self::display_small(was),
            Self::display_small(is_now)
        )
    }

    fn set_segment(&mut self, r: SegmentRegister, value: u16) -> String {
        let was = self.get_segment(r);
        match r {
            SegmentRegister::Code => self.registers.cs = value,
            SegmentRegister::Data => self.registers.ds = value,
            SegmentRegister::Stack => self.registers.ss = value,
            SegmentRegister::Extra => self.registers.es = value,
        }
        let is_now = self.get_segment(r);
        format!(
            "{}:{}->{}",
            r,
            Self::display_small(was),
            Self::display_small(is_now)
        )
    }

    fn get_memory_byte(&self, index: usize) -> u8 {
        self.memory[index]
    }

    fn get_memory_word(&self, index: usize) -> u16 {
        self.memory[index] as u16 * 256 + self.memory[index + 1] as u16
    }

    fn set_memory_byte(&mut self, index: usize, value: u8) -> String {
        self.memory[index] = value;
        "todo".to_owned()
    }

    fn set_memory_word(&mut self, index: usize, value: u16) -> String {
        self.memory[index] = (value % 256) as u8;
        self.memory[index + 1] = (value / 256) as u8;
        "todo".to_owned()
    }

    fn resolve_eaddr(&self, a: &EffectiveAddress) -> usize {
        match a {
            EffectiveAddress::Sum(WithOffset::Basic((base, source_dest))) => {
                let base_offset = match base {
                    Base::Bp => self.get_register(&Register::Special(SpecialRegister::BasePointer)),
                    Base::Bx => self
                        .get_register(&Register::General(GeneralRegister::B, RegisterSubset::All)),
                };
                let source_offset = match source_dest {
                    SourceDest::Source => {
                        self.get_register(&Register::Special(SpecialRegister::SourceIndex))
                    }
                    SourceDest::Dest => {
                        self.get_register(&Register::Special(SpecialRegister::DestIndex))
                    }
                };
                base_offset as usize + source_offset as usize
            }
            EffectiveAddress::Sum(WithOffset::WithU8((base, source_dest), addend)) => {
                let base_offset = match base {
                    Base::Bp => self.get_register(&Register::Special(SpecialRegister::BasePointer)),
                    Base::Bx => self
                        .get_register(&Register::General(GeneralRegister::B, RegisterSubset::All)),
                };
                let source_offset = match source_dest {
                    SourceDest::Source => {
                        self.get_register(&Register::Special(SpecialRegister::SourceIndex))
                    }
                    SourceDest::Dest => {
                        self.get_register(&Register::Special(SpecialRegister::DestIndex))
                    }
                };
                base_offset as usize + source_offset as usize + *addend as usize
            }
            EffectiveAddress::Sum(WithOffset::WithU16((base, source_dest), addend)) => {
                let base_offset = match base {
                    Base::Bp => self.get_register(&Register::Special(SpecialRegister::BasePointer)),
                    Base::Bx => self
                        .get_register(&Register::General(GeneralRegister::B, RegisterSubset::All)),
                };
                let source_offset = match source_dest {
                    SourceDest::Source => {
                        self.get_register(&Register::Special(SpecialRegister::SourceIndex))
                    }
                    SourceDest::Dest => {
                        self.get_register(&Register::Special(SpecialRegister::DestIndex))
                    }
                };
                base_offset as usize + source_offset as usize + *addend as usize
            }
            EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)) => {
                let source_offset = match source_dest {
                    SourceDest::Source => {
                        self.get_register(&Register::Special(SpecialRegister::SourceIndex))
                    }
                    SourceDest::Dest => {
                        self.get_register(&Register::Special(SpecialRegister::DestIndex))
                    }
                };
                source_offset as usize
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, offset)) => {
                let source_offset = match source_dest {
                    SourceDest::Source => {
                        self.get_register(&Register::Special(SpecialRegister::SourceIndex))
                    }
                    SourceDest::Dest => {
                        self.get_register(&Register::Special(SpecialRegister::DestIndex))
                    }
                };
                source_offset as usize + *offset as usize
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU16(source_dest, offset)) => {
                let source_offset = match source_dest {
                    SourceDest::Source => {
                        self.get_register(&Register::Special(SpecialRegister::SourceIndex))
                    }
                    SourceDest::Dest => {
                        self.get_register(&Register::Special(SpecialRegister::DestIndex))
                    }
                };
                source_offset as usize + *offset as usize
            }
            EffectiveAddress::Bx(WithOffset::Basic(())) => {
                let bx =
                    self.get_register(&Register::General(GeneralRegister::B, RegisterSubset::All));
                bx as usize
            }
            EffectiveAddress::Bx(WithOffset::WithU8((), offset)) => {
                let bx =
                    self.get_register(&Register::General(GeneralRegister::B, RegisterSubset::All));
                bx as usize + *offset as usize
            }
            EffectiveAddress::Bx(WithOffset::WithU16((), offset)) => {
                let bx =
                    self.get_register(&Register::General(GeneralRegister::B, RegisterSubset::All));
                bx as usize + *offset as usize
            }
            EffectiveAddress::Direct(addr) => *addr as usize,
            EffectiveAddress::BasePointer(offset) => {
                let bp = self.get_register(&Register::Special(SpecialRegister::BasePointer));
                bp as usize + *offset as usize
            }
            EffectiveAddress::BasePointerWide(offset) => {
                let bp = self.get_register(&Register::Special(SpecialRegister::BasePointer));
                bp as usize + *offset as usize
            }
        }
    }

    fn step_mov(&mut self, instruction: &MoveInstruction) -> String {
        let preamble = format!("{}", instruction);
        let description = match &instruction {
            MoveInstruction::RegRegMove(mov) => {
                let value = self.get_register(&mov.source);
                self.set_register(&mov.dest, value)
            }
            MoveInstruction::RegMemMove(mov) => {
                let value = self.get_register(&mov.source);
                if mov.source.is_wide() {
                    self.set_memory_word(self.resolve_eaddr(&mov.dest), value)
                } else {
                    self.set_memory_byte(self.resolve_eaddr(&mov.dest), value as u8)
                }
            }
            MoveInstruction::MemRegMove(mov) => {
                if mov.dest.is_wide() {
                    let value = self.get_memory_word(self.resolve_eaddr(&mov.source));
                    self.set_register(&mov.dest, value)
                } else {
                    let value = self.get_memory_byte(self.resolve_eaddr(&mov.source));
                    self.set_register(&mov.dest, value as u16)
                }
            }
            MoveInstruction::ImmediateToRegister(mov) => match mov {
                ImmediateToRegister::Byte(dest, value) => self.set_register(dest, *value as u16),
                ImmediateToRegister::Wide(dest, value) => self.set_register(dest, *value),
            },
            MoveInstruction::ImmediateToMemory(mov) => match mov {
                ImmediateToMemory::Byte(addr, value) => {
                    let dest = self.resolve_eaddr(addr);
                    self.set_memory_byte(dest, *value)
                }
                ImmediateToMemory::Word(addr, value) => {
                    let dest = self.resolve_eaddr(addr);
                    self.set_memory_word(dest, *value)
                }
            },
            MoveInstruction::MemoryToAccumulator(mov) => {
                if mov.is_wide {
                    self.set_register(
                        &Register::General(GeneralRegister::A, RegisterSubset::All),
                        self.get_memory_word(mov.address as usize),
                    )
                } else {
                    self.set_register(
                        &Register::General(
                            GeneralRegister::A,
                            RegisterSubset::Subset(ByteRegisterSubset::Low),
                        ),
                        self.get_memory_byte(mov.address as usize) as u16,
                    )
                }
            }
            MoveInstruction::AccumulatorToMemory(mov) => {
                if mov.is_wide {
                    self.set_memory_word(
                        mov.address as usize,
                        self.get_register(&Register::General(
                            GeneralRegister::A,
                            RegisterSubset::All,
                        )),
                    )
                } else {
                    self.set_memory_byte(
                        mov.address as usize,
                        self.get_register(&Register::General(
                            GeneralRegister::A,
                            RegisterSubset::Subset(ByteRegisterSubset::Low),
                        )) as u8,
                    )
                }
            }
            MoveInstruction::SegmentToMemory(mov) => {
                let v = self.get_segment(mov.source);
                let dest = self.resolve_eaddr(&mov.dest);
                self.set_memory_word(dest, v)
            }
            MoveInstruction::MemoryToSegment(mov) => {
                let value = self.get_memory_word(self.resolve_eaddr(&mov.source));
                self.set_segment(mov.dest, value)
            }
            MoveInstruction::SegmentToRegister(mov) => {
                let v = self.get_segment(mov.source);
                self.set_register(&mov.dest, v)
            }
            MoveInstruction::RegisterToSegment(mov) => {
                let value = self.get_register(&mov.source);
                self.set_segment(mov.dest, value)
            }
        };
        format!("{} ; {}", preamble, description)
    }

    /// Returns a string representation of what happened.
    pub fn step(&mut self, instruction: &Instruction<i8>) -> String {
        match instruction {
            Instruction::Move(mov) => self.step_mov(mov),
            Instruction::Arithmetic(_) => todo!(),
            Instruction::Jump(_, _) => todo!(),
            Instruction::Trivia(_) => format!("{}", instruction),
        }
    }

    pub fn dump_register_state(&self) -> String {
        let mut result = "".to_owned();
        for r in [
            GeneralRegister::A,
            GeneralRegister::B,
            GeneralRegister::C,
            GeneralRegister::D,
        ] {
            let value = self.get_register(&Register::General(r.clone(), RegisterSubset::All));
            result.push_str(&format!(
                "{}x: {} ({})\n",
                r,
                Self::display_big(value),
                value
            ))
        }

        for r in [
            SpecialRegister::StackPointer,
            SpecialRegister::BasePointer,
            SpecialRegister::SourceIndex,
            SpecialRegister::DestIndex,
        ] {
            let value = self.get_register(&Register::Special(r.clone()));
            result.push_str(&format!(
                "{}: {} ({})\n",
                r,
                Self::display_big(value),
                value
            ))
        }

        for r in [
            SegmentRegister::Extra,
            SegmentRegister::Code,
            SegmentRegister::Stack,
            SegmentRegister::Data,
        ] {
            let value = self.get_segment(r);
            if value != 0 {
                result.push_str(&format!(
                    "{}: {} ({})\n",
                    r,
                    Self::display_big(value),
                    value
                ));
            }
        }

        result
    }
}
