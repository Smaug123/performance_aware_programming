use std::fmt::Display;

use crate::boolean_instruction::BooleanInstruction;
use crate::inc_instruction::IncInstruction;
use crate::{
    arithmetic_instruction::{
        ArithmeticInstruction, ArithmeticInstructionSelect, ArithmeticOperation,
    },
    effective_address::{EffectiveAddress, WithOffset},
    instruction::Instruction,
    jump_instruction::Jump,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ControlFlag {
    Trap,
    Direction,
    InterruptEnable,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StatusFlag {
    Overflow,
    Sign,
    Zero,
    AuxiliaryCarry,
    Parity,
    Carry,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Flag {
    Control(ControlFlag),
    Status(StatusFlag),
}

impl Flag {
    const fn to_position(self) -> u8 {
        match self {
            Flag::Control(ControlFlag::Direction) => 0,
            Flag::Control(ControlFlag::InterruptEnable) => 1,
            Flag::Control(ControlFlag::Trap) => 2,
            Flag::Status(StatusFlag::AuxiliaryCarry) => 3,
            Flag::Status(StatusFlag::Carry) => 4,
            Flag::Status(StatusFlag::Overflow) => 5,
            Flag::Status(StatusFlag::Parity) => 6,
            Flag::Status(StatusFlag::Sign) => 7,
            Flag::Status(StatusFlag::Zero) => 8,
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Flags {
    fields: u16,
}

impl Display for Flags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let carry = if self.get(Flag::Status(StatusFlag::Carry)) {
            "C"
        } else {
            ""
        };
        let parity = if self.get(Flag::Status(StatusFlag::Parity)) {
            "P"
        } else {
            ""
        };
        let auxiliary_carry = if self.get(Flag::Status(StatusFlag::AuxiliaryCarry)) {
            "A"
        } else {
            ""
        };
        let sign = if self.get(Flag::Status(StatusFlag::Sign)) {
            "S"
        } else {
            ""
        };
        let of = if self.get(Flag::Status(StatusFlag::Overflow)) {
            "O"
        } else {
            ""
        };
        let zero = if self.get(Flag::Status(StatusFlag::Zero)) {
            "Z"
        } else {
            ""
        };
        f.write_fmt(format_args!(
            "{carry}{parity}{auxiliary_carry}{sign}{of}{zero}"
        ))
    }
}

impl Flags {
    const fn nth_flag(v: u16, n: u8) -> bool {
        v & (1 << n) > 0
    }

    const fn get(self, f: Flag) -> bool {
        let all_flags = self.fields;
        Self::nth_flag(all_flags, f.to_position())
    }

    fn set(&mut self, f: Flag, v: bool) {
        if v {
            self.fields |= 1 << f.to_position()
        } else {
            self.fields &= u16::MAX - (1 << f.to_position())
        }
    }
}

pub struct Computer {
    program_counter: u16,
    flags: Flags,
    registers: Registers,
    #[allow(dead_code)]
    memory: [u8; 65536],
}

struct ResultFlags {
    auxiliary_carry: bool,
    overflow: bool,
    should_write: bool,
    carry: bool,
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
            flags: Flags { fields: 0 },
            program_counter: 0,
        }
    }

    fn set_flag(&mut self, f: Flag, v: bool) {
        self.flags.set(f, v)
    }

    pub fn dump_flag_state(&self) -> String {
        format!("{}", self.flags)
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
        if was != is_now {
            format!(
                "{}:{}->{}",
                register_for_print,
                Self::display_small(was),
                Self::display_small(is_now)
            )
        } else {
            "".to_owned()
        }
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
        self.memory[index] as u16 + self.memory[index + 1] as u16 * 256
    }

    fn set_memory_byte(&mut self, index: usize, value: u8) -> String {
        self.memory[index] = value;
        "".to_owned()
    }

    fn set_memory_word(&mut self, index: usize, value: u16) -> String {
        self.memory[index] = (value % 256) as u8;
        self.memory[index + 1] = (value / 256) as u8;
        "".to_owned()
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
                    self.set_memory_word(dest, *value as u16)
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

    /// Returns true if the operation overflowed, and true if the value is supposed
    /// to be written to the destination (so `cmp` returns false), and true if there
    /// was a carry out of the low-order nibble.
    fn apply(
        op: ArithmeticOperation,
        to_arg: u16,
        incoming_arg_raw: u16,
        is_byte_extended: bool,
    ) -> (u16, ResultFlags) {
        // If is_byte_extended, the answer is simply "if n>=128, then treat as 256-n".
        // But the flags behave as though it were byte arithmetic.
        let incoming_arg = if is_byte_extended && incoming_arg_raw >= 128 {
            u16::MAX - (255 - incoming_arg_raw)
        } else {
            incoming_arg_raw
        };
        match op {
            ArithmeticOperation::Add => {
                let result = to_arg as u32 + incoming_arg as u32;
                let result_flags = ResultFlags {
                    overflow: (to_arg >= 1 << 15 && incoming_arg >= 1 << 15 && result < 1 << 15)
                        || (to_arg < 1 << 15 && incoming_arg < 1 << 15 && result > 1 << 15),
                    auxiliary_carry: (to_arg % 16) + (incoming_arg_raw % 16) >= 16,
                    should_write: true,
                    carry: result > u16::MAX as u32,
                };
                ((result % (u16::MAX as u32 + 1)) as u16, result_flags)
            }
            ArithmeticOperation::Or => (
                to_arg | incoming_arg,
                ResultFlags {
                    overflow: false,
                    should_write: true,
                    carry: false,
                    auxiliary_carry: false,
                },
            ),
            ArithmeticOperation::AddWithCarry => todo!(),
            ArithmeticOperation::SubWithBorrow => todo!(),
            ArithmeticOperation::And => (
                to_arg & incoming_arg,
                ResultFlags {
                    overflow: false,
                    should_write: true,
                    auxiliary_carry: false,
                    carry: true,
                },
            ),
            ArithmeticOperation::Sub => {
                let auxiliary_carry = to_arg % 16 < incoming_arg_raw % 16;
                if to_arg < incoming_arg {
                    let result = u16::MAX - (incoming_arg - to_arg) + 1;
                    (
                        result,
                        ResultFlags {
                            should_write: true,
                            overflow: (result < 1 << 15 && to_arg >= 1 << 15),
                            auxiliary_carry,
                            carry: true,
                        },
                    )
                } else {
                    // We might be wrapping around 0, so check for overflow separately.
                    let result = to_arg - incoming_arg;
                    (
                        result,
                        ResultFlags {
                            should_write: true,
                            overflow: (result < 1 << 15 && to_arg >= 1 << 15),
                            auxiliary_carry,
                            carry: false,
                        },
                    )
                }
            }
            ArithmeticOperation::Xor => (
                to_arg ^ incoming_arg,
                ResultFlags {
                    overflow: false,
                    should_write: true,
                    auxiliary_carry: false,
                    carry: false,
                },
            ),
            ArithmeticOperation::Cmp => {
                // CPAS can all be set by Cmp
                let auxiliary_carry = to_arg % 16 < incoming_arg_raw % 16;
                if to_arg < incoming_arg {
                    let result = u16::MAX - (incoming_arg - to_arg) + 1;
                    (
                        result,
                        ResultFlags {
                            should_write: false,
                            auxiliary_carry,
                            overflow: (result < 1 << 15 && to_arg >= 1 << 15),
                            carry: true,
                        },
                    )
                } else {
                    let result = to_arg - incoming_arg;
                    (
                        result,
                        ResultFlags {
                            should_write: false,
                            auxiliary_carry,
                            overflow: (result < 1 << 15 && to_arg >= 1 << 15),
                            carry: false,
                        },
                    )
                }
            }
        }
    }

    /// true if v has an odd number of bits in
    const fn is_odd_parity(v: u16) -> bool {
        let mut parity = 0u8;
        let mut v = v;
        while v > 0 {
            if v % 2 == 1 {
                parity += 1;
            }
            v >>= 1;
        }
        parity % 2 == 1
    }

    fn step_arithmetic(&mut self, instruction: &ArithmeticInstruction) -> String {
        let (source, old_value, new_value, flags) = match &instruction.instruction {
            ArithmeticInstructionSelect::RegisterToRegister(instr) => {
                let current_value = self.get_register(&instr.dest);
                let incoming_value = self.get_register(&instr.source);
                let (new_value, flags) =
                    Self::apply(instruction.op, current_value, incoming_value, false);
                if flags.should_write {
                    self.set_register(&instr.dest, new_value);
                }
                (format!("{}", instr.dest), current_value, new_value, flags)
            }
            ArithmeticInstructionSelect::RegisterToMemory(_) => todo!(),
            ArithmeticInstructionSelect::MemoryToRegister(instr) => {
                let current_value = self.get_register(&instr.dest);
                let incoming_value = if instr.dest.is_wide() {
                    self.get_memory_word(self.resolve_eaddr(&instr.source))
                } else {
                    self.get_memory_byte(self.resolve_eaddr(&instr.source)) as u16
                };
                let (new_value, flags) =
                    Self::apply(instruction.op, current_value, incoming_value, false);
                if flags.should_write {
                    self.set_register(&instr.dest, new_value);
                }
                (format!("{}", instr.dest), current_value, new_value, flags)
            }
            ArithmeticInstructionSelect::ImmediateToRegisterByte(register, value, is_extended) => {
                let current_value = self.get_register(register);
                let (new_value, flags) = if *is_extended {
                    Self::apply(instruction.op, current_value, *value as u16, *is_extended)
                    // Self::apply(instruction.op, current_value, *value as u16)
                } else {
                    todo!()
                };
                if flags.should_write {
                    self.set_register(register, new_value);
                }
                (format!("{}", register), current_value, new_value, flags)
            }
            ArithmeticInstructionSelect::ImmediateToRegisterWord(register, value, is_extended) => {
                let current_value = self.get_register(register);
                let (new_value, flags) =
                    Self::apply(instruction.op, current_value, *value, *is_extended);
                if flags.should_write {
                    self.set_register(register, new_value);
                }
                (format!("{}", register), current_value, new_value, flags)
            }
            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(_, _, _) => todo!(),
            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(_, _) => todo!(),
            ArithmeticInstructionSelect::ImmediateToAccByte(value) => {
                let reg = Register::General(
                    GeneralRegister::A,
                    RegisterSubset::Subset(ByteRegisterSubset::Low),
                );
                let current_value = self.get_register(&reg);
                let (new_value, flags) =
                    Self::apply(instruction.op, current_value, *value as u16, false);
                if flags.should_write {
                    self.set_register(&reg, new_value);
                }
                (format!("{}", reg), current_value, new_value, flags)
            }
            ArithmeticInstructionSelect::ImmediateToAccWord(value) => {
                let reg = Register::General(GeneralRegister::A, RegisterSubset::All);
                let current_value = self.get_register(&reg);
                let (new_value, flags) = Self::apply(instruction.op, current_value, *value, false);
                if flags.should_write {
                    self.set_register(&reg, new_value);
                }
                (format!("{}", reg), current_value, new_value, flags)
            }
        };

        self.set_flag(Flag::Status(StatusFlag::Zero), new_value == 0);
        self.set_flag(Flag::Status(StatusFlag::Overflow), flags.overflow);
        // TODO: what if this was a byte instruction instead
        self.set_flag(Flag::Status(StatusFlag::Sign), new_value & 0x8000 > 0);
        self.set_flag(
            Flag::Status(StatusFlag::AuxiliaryCarry),
            flags.auxiliary_carry,
        );
        self.set_flag(Flag::Status(StatusFlag::Carry), flags.carry);
        self.set_flag(
            Flag::Status(StatusFlag::Parity),
            !Self::is_odd_parity(new_value % 256),
        );
        let result_desc = if flags.should_write && old_value != new_value {
            format!(
                " {}:{}->{}",
                source,
                Self::display_small(old_value),
                Self::display_small(new_value)
            )
        } else {
            "".to_owned()
        };
        format!("{instruction} ;{result_desc}")
    }

    fn step_boolean(&mut self, _instruction: &BooleanInstruction) -> String {
        todo!()
    }

    fn step_inc(&mut self, instruction: &IncInstruction) -> String {
        let result_desc = match instruction {
            IncInstruction::Register(reg) => {
                let old_value = self.get_register(reg);
                let new_value = if old_value == u16::MAX {
                    self.set_flag(Flag::Status(StatusFlag::Overflow), true);
                    0
                } else {
                    self.set_flag(Flag::Status(StatusFlag::Overflow), false);
                    old_value + 1
                };

                self.set_register(reg, new_value)
            }
            IncInstruction::Memory(addr) => {
                let location = self.resolve_eaddr(addr);
                let old_value = self.get_memory_word(location);
                let new_value = if old_value == u16::MAX {
                    self.set_flag(Flag::Status(StatusFlag::Overflow), true);
                    0
                } else {
                    self.set_flag(Flag::Status(StatusFlag::Overflow), false);
                    old_value + 1
                };

                self.set_memory_word(location, new_value)
            }
        };

        format!("{instruction} ;{result_desc}")
    }

    fn step_ret(&mut self) -> String {
        todo!()
    }

    fn step_jump(&mut self, jump: Jump, offset: i8) -> String {
        let mut reg_desc = "".to_owned();
        let should_jump = match jump {
            Jump::Je => self.flags.get(Flag::Status(StatusFlag::Zero)),
            Jump::Jl => {
                self.flags.get(Flag::Status(StatusFlag::Overflow))
                    ^ self.flags.get(Flag::Status(StatusFlag::Sign))
            }
            Jump::Jle => {
                (self.flags.get(Flag::Status(StatusFlag::Overflow))
                    ^ self.flags.get(Flag::Status(StatusFlag::Sign)))
                    | (self.flags.get(Flag::Status(StatusFlag::Zero)))
            }
            Jump::Jb => self.flags.get(Flag::Status(StatusFlag::Carry)),
            Jump::Jbe => {
                self.flags.get(Flag::Status(StatusFlag::Zero))
                    | self.flags.get(Flag::Status(StatusFlag::Carry))
            }
            Jump::Jp => self.flags.get(Flag::Status(StatusFlag::Parity)),
            Jump::Jo => self.flags.get(Flag::Status(StatusFlag::Overflow)),
            Jump::Js => self.flags.get(Flag::Status(StatusFlag::Sign)),
            Jump::Jne => !self.flags.get(Flag::Status(StatusFlag::Zero)),
            Jump::Jnl => {
                self.flags.get(Flag::Status(StatusFlag::Overflow))
                    ^ self.flags.get(Flag::Status(StatusFlag::Sign))
            }
            Jump::Jnle => {
                !((self.flags.get(Flag::Status(StatusFlag::Overflow))
                    ^ self.flags.get(Flag::Status(StatusFlag::Sign)))
                    | (self.flags.get(Flag::Status(StatusFlag::Zero))))
            }
            Jump::Jnb => !self.flags.get(Flag::Status(StatusFlag::Carry)),
            Jump::Jnbe => {
                !(self.flags.get(Flag::Status(StatusFlag::Zero))
                    | self.flags.get(Flag::Status(StatusFlag::Carry)))
            }
            Jump::Jnp => !self.flags.get(Flag::Status(StatusFlag::Parity)),
            Jump::Jno => !self.flags.get(Flag::Status(StatusFlag::Overflow)),
            Jump::Jns => !self.flags.get(Flag::Status(StatusFlag::Sign)),
            Jump::Loop => {
                let reg = Register::General(GeneralRegister::C, RegisterSubset::All);
                let prev = self.get_register(&reg);
                let new_value = if prev == 0 {
                    self.set_register(&reg, u16::MAX);
                    u16::MAX
                } else {
                    self.set_register(&reg, prev - 1);
                    prev - 1
                };
                reg_desc.push_str(&format!(
                    " ; cx:{}->{}",
                    Self::display_small(prev),
                    Self::display_small(new_value)
                ));
                prev != 1
            }
            Jump::Loopz => {
                let reg = Register::General(GeneralRegister::C, RegisterSubset::All);
                let prev = self.get_register(&reg);
                self.set_register(&reg, prev - 1);
                reg_desc.push_str(&format!(
                    " ; cx:{}->{}",
                    Self::display_small(prev),
                    Self::display_small(prev - 1)
                ));
                prev != 1 && self.flags.get(Flag::Status(StatusFlag::Zero))
            }
            Jump::Loopnz => {
                let reg = Register::General(GeneralRegister::C, RegisterSubset::All);
                let prev = self.get_register(&reg);
                self.set_register(&reg, prev - 1);
                reg_desc.push_str(&format!(
                    " ; cx:{}->{}",
                    Self::display_small(prev),
                    Self::display_small(prev - 1)
                ));
                prev != 1 && !self.flags.get(Flag::Status(StatusFlag::Zero))
            }
            Jump::Jcxz => {
                let reg = Register::General(GeneralRegister::C, RegisterSubset::All);
                self.get_register(&reg) == 0
            }
        };

        if should_jump {
            self.program_counter = (self.program_counter as i32 + offset as i32) as u16;
        }
        // In NASM, the dollar sign is an offset *without* including the bytes of the jump.
        format!(
            "{} ${}{}{}",
            jump,
            if offset > 0 { "+" } else { "" },
            offset + 2,
            reg_desc,
        )
    }

    pub fn get_program_counter(&self) -> u16 {
        self.program_counter
    }

    /// Returns a string representation of what happened.
    pub fn step(&mut self, instruction: &Instruction<i8>, display_ip: bool) -> String {
        let advance = instruction.length();
        let old_ip = if display_ip {
            Some(self.program_counter)
        } else {
            None
        };
        let old_flags = self.flags;
        self.program_counter += advance as u16;
        let step_desc = match instruction {
            Instruction::Move(mov) => self.step_mov(mov),
            Instruction::Arithmetic(arith) => self.step_arithmetic(arith),
            Instruction::Jump(jump, offset) => self.step_jump(*jump, *offset),
            Instruction::Boolean(boolean) => self.step_boolean(boolean),
            Instruction::Inc(inc) => self.step_inc(inc),
            Instruction::Ret => self.step_ret(),
            Instruction::Trivia(_) => format!("{}", instruction),
        };

        let flags_desc = if old_flags == self.flags {
            "".to_owned()
        } else {
            format!(" flags:{}->{}", old_flags, self.flags)
        };
        let ip_desc = match old_ip {
            None => "".to_owned(),
            Some(old_ip) => {
                format!(
                    "ip:{}->{}",
                    //if reg_desc.is_empty() { " ; " } else { " " },
                    Self::display_small(old_ip),
                    Self::display_small(self.program_counter)
                )
            }
        };

        format!("{}{}{}", step_desc, ip_desc, flags_desc)
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
            if value != 0 {
                result.push_str(&format!(
                    "{}x: {} ({})\n",
                    r,
                    Self::display_big(value),
                    value
                ))
            }
        }

        for r in [
            SpecialRegister::StackPointer,
            SpecialRegister::BasePointer,
            SpecialRegister::SourceIndex,
            SpecialRegister::DestIndex,
        ] {
            let value = self.get_register(&Register::Special(r.clone()));
            if value != 0 {
                result.push_str(&format!(
                    "{}: {} ({})\n",
                    r,
                    Self::display_big(value),
                    value
                ))
            }
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

#[cfg(test)]
mod test_computer {
    use crate::computer::Computer;

    #[test]
    fn test_parity() {
        assert!(!Computer::is_odd_parity(0));
        assert!(Computer::is_odd_parity(1));
        assert!(Computer::is_odd_parity(2));
        assert!(!Computer::is_odd_parity(3));
        assert!(!Computer::is_odd_parity(0x7ea));
    }
}
