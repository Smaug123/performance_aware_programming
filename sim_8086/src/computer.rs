use std::fmt::Display;

use crate::boolean_instruction::{
    BooleanInstruction, BooleanInstructionDestination, BooleanInstructionType, ImmediateToAcc,
};
use crate::inc_instruction::IncInstruction;
use crate::logic_instruction::{LogicInstruction, LogicInstructionType, LogicTarget};
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

#[derive(Clone, Eq, PartialEq)]
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

impl Registers {
    pub fn diff(&self, old: &Registers) -> String {
        let mut result = Vec::new();

        if self.a != old.a {
            result.push(format!(
                "ax:{}->{}",
                display_small(old.a),
                display_small(self.a)
            ));
        }
        if self.b != old.b {
            result.push(format!(
                "bx:{}->{}",
                display_small(old.b),
                display_small(self.b)
            ));
        }
        if self.c != old.c {
            result.push(format!(
                "cx:{}->{}",
                display_small(old.c),
                display_small(self.c)
            ));
        }
        if self.d != old.d {
            result.push(format!(
                "dx:{}->{}",
                display_small(old.d),
                display_small(self.d)
            ));
        }
        if self.si != old.si {
            result.push(format!(
                "si:{}->{}",
                display_small(old.si),
                display_small(self.si)
            ));
        }
        if self.di != old.di {
            result.push(format!(
                "di:{}->{}",
                display_small(old.di),
                display_small(self.di)
            ));
        }
        if self.sp != old.sp {
            result.push(format!(
                "sp:{}->{}",
                display_small(old.sp),
                display_small(self.sp)
            ));
        }
        if self.bp != old.bp {
            result.push(format!(
                "bp:{}->{}",
                display_small(old.bp),
                display_small(self.bp)
            ));
        }
        if self.ss != old.ss {
            result.push(format!(
                "ss:{}->{}",
                display_small(old.ss),
                display_small(self.ss)
            ));
        }
        if self.cs != old.cs {
            result.push(format!(
                "cs:{}->{}",
                display_small(old.cs),
                display_small(self.cs)
            ));
        }
        if self.es != old.es {
            result.push(format!(
                "es:{}->{}",
                display_small(old.es),
                display_small(self.es)
            ));
        }
        if self.ds != old.ds {
            result.push(format!(
                "ds:{}->{}",
                display_small(old.ds),
                display_small(self.ds)
            ));
        }

        result.join(" ; ")
    }
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
    clocks_executed: u32,
}

struct ResultFlags {
    auxiliary_carry: bool,
    overflow: bool,
    sign: bool,
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
            clocks_executed: 0,
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

    fn set_register(&mut self, r: &Register, value: u16) {
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
    }

    fn set_segment(&mut self, r: SegmentRegister, value: u16) {
        match r {
            SegmentRegister::Code => self.registers.cs = value,
            SegmentRegister::Data => self.registers.ds = value,
            SegmentRegister::Stack => self.registers.ss = value,
            SegmentRegister::Extra => self.registers.es = value,
        }
    }

    fn get_memory_byte(&self, index: usize) -> u8 {
        self.memory[index]
    }

    /// Returns true if the access took an extra four cycles.
    fn get_memory_word(&self, index: usize) -> (u16, bool) {
        let value = self.memory[index] as u16 + self.memory[index + 1] as u16 * 256;
        (value, index % 2 == 1)
    }

    fn set_memory_byte(&mut self, index: usize, value: u8) {
        self.memory[index] = value;
    }

    /// Returns true if the access took an extra four cycles.
    fn set_memory_word(&mut self, index: usize, value: u16) -> bool {
        self.memory[index] = (value % 256) as u8;
        self.memory[index + 1] = (value / 256) as u8;
        index % 2 == 1
    }

    fn get_base_offset(&self, base: &Base) -> u16 {
        match base {
            Base::Bp => self.get_register(&Register::Special(SpecialRegister::BasePointer)),
            Base::Bx => {
                self.get_register(&Register::General(GeneralRegister::B, RegisterSubset::All))
            }
        }
    }

    fn get_source_offset(&self, source_dest: &SourceDest) -> u16 {
        match source_dest {
            SourceDest::Source => {
                self.get_register(&Register::Special(SpecialRegister::SourceIndex))
            }
            SourceDest::Dest => self.get_register(&Register::Special(SpecialRegister::DestIndex)),
        }
    }

    fn resolve_eaddr(&self, a: &EffectiveAddress) -> usize {
        match a {
            EffectiveAddress::Sum(WithOffset::Basic((base, source_dest))) => {
                let base_offset = self.get_base_offset(base);
                let source_offset = self.get_source_offset(source_dest);
                base_offset as usize + source_offset as usize
            }
            EffectiveAddress::Sum(WithOffset::WithU8((base, source_dest), addend)) => {
                let base_offset = self.get_base_offset(base);
                let source_offset = self.get_source_offset(source_dest);
                base_offset as usize + source_offset as usize + *addend as usize
            }
            EffectiveAddress::Sum(WithOffset::WithU16((base, source_dest), addend)) => {
                let base_offset = self.get_base_offset(base);
                let source_offset = self.get_source_offset(source_dest);
                base_offset as usize + source_offset as usize + *addend as usize
            }
            EffectiveAddress::SpecifiedIn(WithOffset::Basic(source_dest)) => {
                let source_offset = self.get_source_offset(source_dest);
                source_offset as usize
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU8(source_dest, offset)) => {
                let source_offset = self.get_source_offset(source_dest);
                source_offset as usize + *offset as usize
            }
            EffectiveAddress::SpecifiedIn(WithOffset::WithU16(source_dest, offset)) => {
                let source_offset = self.get_source_offset(source_dest);
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

    /// Returns true if the access took four extra clocks.
    fn step_mov(&mut self, instruction: &MoveInstruction) -> bool {
        match &instruction {
            MoveInstruction::RegRegMove(mov) => {
                let value = self.get_register(&mov.source);
                self.set_register(&mov.dest, value);
                false
            }
            MoveInstruction::RegMemMove(mov) => {
                let value = self.get_register(&mov.source);
                if mov.source.is_wide() {
                    self.set_memory_word(self.resolve_eaddr(&mov.dest), value)
                } else {
                    self.set_memory_byte(self.resolve_eaddr(&mov.dest), value as u8);
                    false
                }
            }
            MoveInstruction::MemRegMove(mov) => {
                if mov.dest.is_wide() {
                    let (value, slow) = self.get_memory_word(self.resolve_eaddr(&mov.source));
                    self.set_register(&mov.dest, value);
                    slow
                } else {
                    let value = self.get_memory_byte(self.resolve_eaddr(&mov.source));
                    self.set_register(&mov.dest, value as u16);
                    false
                }
            }
            MoveInstruction::ImmediateToRegister(mov) => {
                match mov {
                    ImmediateToRegister::Byte(dest, value) => {
                        self.set_register(dest, *value as u16)
                    }
                    ImmediateToRegister::Wide(dest, value) => self.set_register(dest, *value),
                }
                false
            }
            MoveInstruction::ImmediateToMemory(mov) => match mov {
                ImmediateToMemory::Byte(addr, value) => {
                    let dest = self.resolve_eaddr(addr);
                    self.set_memory_byte(dest, *value);
                    false
                }
                ImmediateToMemory::Word(addr, value) => {
                    let dest = self.resolve_eaddr(addr);
                    self.set_memory_word(dest, *value)
                }
            },
            MoveInstruction::MemoryToAccumulator(mov) => {
                if mov.is_wide {
                    let (value, slow) = self.get_memory_word(mov.address as usize);
                    self.set_register(
                        &Register::General(GeneralRegister::A, RegisterSubset::All),
                        value,
                    );
                    slow
                } else {
                    self.set_register(
                        &Register::General(
                            GeneralRegister::A,
                            RegisterSubset::Subset(ByteRegisterSubset::Low),
                        ),
                        self.get_memory_byte(mov.address as usize) as u16,
                    );
                    false
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
                    );
                    false
                }
            }
            MoveInstruction::SegmentToMemory(mov) => {
                let v = self.get_segment(mov.source);
                let dest = self.resolve_eaddr(&mov.dest);
                self.set_memory_word(dest, v)
            }
            MoveInstruction::MemoryToSegment(mov) => {
                let (value, slow) = self.get_memory_word(self.resolve_eaddr(&mov.source));
                self.set_segment(mov.dest, value);
                slow
            }
            MoveInstruction::SegmentToRegister(mov) => {
                let v = self.get_segment(mov.source);
                self.set_register(&mov.dest, v);
                false
            }
            MoveInstruction::RegisterToSegment(mov) => {
                let value = self.get_register(&mov.source);
                self.set_segment(mov.dest, value);
                false
            }
        }
    }

    fn apply_u8(op: ArithmeticOperation, to_arg: u8, incoming_arg_raw: u8) -> (u8, ResultFlags) {
        let incoming_arg = if incoming_arg_raw >= 128 {
            u8::MAX - (255 - incoming_arg_raw)
        } else {
            incoming_arg_raw
        };

        match op {
            ArithmeticOperation::Add => {
                let result = to_arg as u16 + incoming_arg as u16;
                let to_ret = (result % (u8::MAX as u16 + 1)) as u8;
                let result_flags = ResultFlags {
                    overflow: (to_arg >= 1 << 7 && incoming_arg >= 1 << 7 && result < 1 << 7)
                        || (to_arg < 1 << 7 && incoming_arg < 1 << 7 && result > 1 << 7),
                    auxiliary_carry: (to_arg % 16) + (incoming_arg_raw % 16) >= 16,
                    should_write: true,
                    carry: result > u8::MAX as u16,
                    sign: to_ret > 1 << 7,
                };
                (to_ret, result_flags)
            }
            ArithmeticOperation::Or => (
                to_arg | incoming_arg,
                ResultFlags {
                    overflow: false,
                    should_write: true,
                    carry: false,
                    auxiliary_carry: false,
                    sign: false,
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
                    sign: false,
                },
            ),
            ArithmeticOperation::Sub => {
                let auxiliary_carry = to_arg % 16 < incoming_arg_raw % 16;
                if to_arg < incoming_arg {
                    let result = u8::MAX - (incoming_arg - to_arg) + 1;
                    (
                        result,
                        ResultFlags {
                            should_write: true,
                            overflow: (result < 1 << 7 && to_arg >= 1 << 7),
                            auxiliary_carry,
                            carry: true,
                            sign: result > 1 << 7,
                        },
                    )
                } else {
                    // We might be wrapping around 0, so check for overflow separately.
                    let result = to_arg - incoming_arg;
                    (
                        result,
                        ResultFlags {
                            should_write: true,
                            overflow: (result < 1 << 7 && to_arg >= 1 << 7),
                            auxiliary_carry,
                            carry: false,
                            sign: result > 1 << 7,
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
                    sign: false,
                },
            ),
            ArithmeticOperation::Cmp => {
                // CPAS can all be set by Cmp
                let auxiliary_carry = to_arg % 16 < incoming_arg_raw % 16;
                if to_arg < incoming_arg {
                    let result = u8::MAX - (incoming_arg - to_arg) + 1;
                    (
                        result,
                        ResultFlags {
                            should_write: false,
                            auxiliary_carry,
                            overflow: (result < 1 << 7 && 7 >= 1 << 7),
                            carry: true,
                            sign: result > 1 << 7,
                        },
                    )
                } else {
                    let result = to_arg - incoming_arg;
                    (
                        result,
                        ResultFlags {
                            should_write: false,
                            auxiliary_carry,
                            overflow: (result < 1 << 7 && to_arg >= 1 << 7),
                            carry: false,
                            sign: result > 1 << 7,
                        },
                    )
                }
            }
        }
    }

    fn apply_u16(
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
                let to_ret = (result % (u16::MAX as u32 + 1)) as u16;
                let result_flags = ResultFlags {
                    overflow: (to_arg >= 1 << 15 && incoming_arg >= 1 << 15 && result < 1 << 15)
                        || (to_arg < 1 << 15 && incoming_arg < 1 << 15 && result > 1 << 15),
                    auxiliary_carry: (to_arg % 16) + (incoming_arg_raw % 16) >= 16,
                    should_write: true,
                    carry: result > u16::MAX as u32,
                    sign: to_ret > 1 << 15,
                };
                (to_ret, result_flags)
            }
            ArithmeticOperation::Or => (
                to_arg | incoming_arg,
                ResultFlags {
                    overflow: false,
                    should_write: true,
                    carry: false,
                    auxiliary_carry: false,
                    sign: false,
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
                    sign: false,
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
                            sign: result > 1 << 15,
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
                            sign: result > 1 << 15,
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
                    sign: false,
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
                            sign: result > 1 << 15,
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
                            sign: result > 1 << 15,
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

    /// Returns a count of the extra cycles caused by misaligned memory access.
    fn step_arithmetic(&mut self, instruction: &ArithmeticInstruction) -> u8 {
        let mut slowness = 0;

        let (new_value, flags) = match &instruction.instruction {
            ArithmeticInstructionSelect::RegisterToRegister(instr) => {
                let current_value = self.get_register(&instr.dest);
                let incoming_value = self.get_register(&instr.source);
                if instr.dest.is_wide() {
                    let (new_value, flags) =
                        Self::apply_u16(instruction.op, current_value, incoming_value, false);
                    if flags.should_write {
                        self.set_register(&instr.dest, new_value);
                    }
                    (new_value, flags)
                } else {
                    let (new_value, flags) =
                        Self::apply_u8(instruction.op, current_value as u8, incoming_value as u8);
                    if flags.should_write {
                        self.set_register(&instr.dest, new_value as u16);
                    }
                    (new_value as u16, flags)
                }
            }
            ArithmeticInstructionSelect::RegisterToMemory(instr) => {
                let dest = self.resolve_eaddr(&instr.dest);
                let (current_value, slow) = if instr.source.is_wide() {
                    self.get_memory_word(dest)
                } else {
                    (self.get_memory_byte(dest) as u16, false)
                };
                if slow {
                    slowness += 4;
                }
                let incoming = self.get_register(&instr.source);

                let (new_value, flags) = if instr.source.is_wide() {
                    Self::apply_u16(instruction.op, current_value, incoming, false)
                } else {
                    let (result, flags) =
                        Self::apply_u8(instruction.op, current_value as u8, incoming as u8);
                    (result as u16, flags)
                };

                if flags.should_write {
                    if instr.source.is_wide() {
                        if self.set_memory_word(dest, new_value) {
                            slowness += 4;
                        }
                    } else {
                        self.set_memory_byte(dest, new_value as u8);
                    }
                }

                (new_value, flags)
            }
            ArithmeticInstructionSelect::MemoryToRegister(instr) => {
                let current_value = self.get_register(&instr.dest);
                let (incoming_value, slow) = if instr.dest.is_wide() {
                    self.get_memory_word(self.resolve_eaddr(&instr.source))
                } else {
                    (
                        self.get_memory_byte(self.resolve_eaddr(&instr.source)) as u16,
                        false,
                    )
                };
                if slow {
                    slowness += 4;
                }
                let (new_value, flags) = if instr.dest.is_wide() {
                    Self::apply_u16(instruction.op, current_value, incoming_value, false)
                } else {
                    let (result, flags) =
                        Self::apply_u8(instruction.op, current_value as u8, incoming_value as u8);
                    (result as u16, flags)
                };
                if flags.should_write {
                    self.set_register(&instr.dest, new_value);
                }
                (new_value, flags)
            }
            ArithmeticInstructionSelect::ImmediateToRegisterByte(register, value, is_extended) => {
                let current_value = self.get_register(register);
                let (new_value, flags) = if *is_extended {
                    Self::apply_u16(instruction.op, current_value, *value as u16, *is_extended)
                    // Self::apply(instruction.op, current_value, *value as u16)
                } else {
                    todo!()
                };
                if flags.should_write {
                    self.set_register(register, new_value);
                }
                (new_value, flags)
            }
            ArithmeticInstructionSelect::ImmediateToRegisterWord(register, value, is_extended) => {
                // TODO: why have we not used is_extended
                let current_value = self.get_register(register);
                let (new_value, flags) =
                    Self::apply_u16(instruction.op, current_value, *value, *is_extended);
                if flags.should_write {
                    self.set_register(register, new_value);
                }
                (new_value, flags)
            }
            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryByte(
                addr,
                value,
                is_extended,
            ) => {
                let location = self.resolve_eaddr(addr);
                let current_value = self.get_memory_byte(location);
                if *is_extended {
                    let (new_value, flags) =
                        Self::apply_u16(instruction.op, current_value as u16, *value as u16, false);
                    if flags.should_write {
                        self.set_memory_byte(location, new_value as u8);
                    }
                    (new_value, flags)
                } else {
                    let (new_value, flags) = Self::apply_u8(instruction.op, current_value, *value);
                    if flags.should_write {
                        self.set_memory_byte(location, new_value);
                    }
                    (new_value as u16, flags)
                }
            }
            ArithmeticInstructionSelect::ImmediateToRegisterOrMemoryWord(_, _) => todo!(),
            ArithmeticInstructionSelect::ImmediateToAccByte(value) => {
                let reg = Register::General(
                    GeneralRegister::A,
                    RegisterSubset::Subset(ByteRegisterSubset::Low),
                );
                let current_value = self.get_register(&reg) as u8;
                let (new_value, flags) = Self::apply_u8(instruction.op, current_value, *value);
                if flags.should_write {
                    self.set_register(&reg, new_value as u16);
                }
                (new_value as u16, flags)
            }
            ArithmeticInstructionSelect::ImmediateToAccWord(value) => {
                let reg = Register::General(GeneralRegister::A, RegisterSubset::All);
                let current_value = self.get_register(&reg);
                let (new_value, flags) =
                    Self::apply_u16(instruction.op, current_value, *value, false);
                if flags.should_write {
                    self.set_register(&reg, new_value);
                }
                (new_value, flags)
            }
        };

        self.set_flag(Flag::Status(StatusFlag::Zero), new_value == 0);
        self.set_flag(Flag::Status(StatusFlag::Overflow), flags.overflow);
        self.set_flag(Flag::Status(StatusFlag::Sign), flags.sign);
        self.set_flag(
            Flag::Status(StatusFlag::AuxiliaryCarry),
            flags.auxiliary_carry,
        );
        self.set_flag(Flag::Status(StatusFlag::Carry), flags.carry);
        self.set_flag(
            Flag::Status(StatusFlag::Parity),
            !Self::is_odd_parity(new_value % 256),
        );

        slowness
    }

    fn compute_boolean(
        &mut self,
        instruction: &BooleanInstructionType,
        op1: u16,
        op2: u16,
    ) -> Option<u16> {
        self.set_flag(Flag::Status(StatusFlag::Overflow), false);
        self.set_flag(Flag::Status(StatusFlag::Carry), false);
        let new_value = match instruction {
            BooleanInstructionType::Test | BooleanInstructionType::And => op1 & op2,
            BooleanInstructionType::Or => op1 | op2,
            BooleanInstructionType::Xor => op1 ^ op2,
        };

        self.set_flags_u16(new_value);

        match instruction {
            BooleanInstructionType::Test => None,
            _ => Some(new_value),
        }
    }

    fn step_boolean(&mut self, instruction: &BooleanInstruction) {
        match &instruction.dest {
            BooleanInstructionDestination::RegReg(reg) => {
                let incoming = self.get_register(&reg.source);
                let current = self.get_register(&reg.dest);
                if let Some(new) = self.compute_boolean(&instruction.selection, incoming, current) {
                    self.set_register(&reg.dest, new)
                }
            }
            BooleanInstructionDestination::ImmediateToAcc(imm) => match imm {
                ImmediateToAcc::Wide(incoming) => {
                    let register = Register::General(GeneralRegister::A, RegisterSubset::All);
                    let current = self.get_register(&register);
                    if let Some(new) =
                        self.compute_boolean(&instruction.selection, *incoming, current)
                    {
                        self.set_register(&register, new);
                    }
                }
                ImmediateToAcc::Narrow(_) => {
                    todo!()
                }
            },
        }
    }

    fn set_flags_u16(&mut self, v: u16) {
        self.set_flag(
            Flag::Status(StatusFlag::Parity),
            !Self::is_odd_parity(v % 256),
        );
        self.set_flag(Flag::Status(StatusFlag::Sign), v > 1 << 15);
        self.set_flag(Flag::Status(StatusFlag::Zero), v == 0);
    }

    fn set_flags_u8(&mut self, v: u8) {
        self.set_flag(
            Flag::Status(StatusFlag::Parity),
            !Self::is_odd_parity(v as u16),
        );
        self.set_flag(Flag::Status(StatusFlag::Sign), v > 1 << 7);
        self.set_flag(Flag::Status(StatusFlag::Zero), v == 0);
    }

    fn step_inc(&mut self, instruction: &IncInstruction) {
        let old_value = self.get_register(&instruction.target);
        let new_value = if instruction.is_inc {
            if old_value == u16::MAX {
                self.set_flag(Flag::Status(StatusFlag::Overflow), true);
                0
            } else {
                self.set_flag(Flag::Status(StatusFlag::Overflow), false);
                old_value + 1
            }
        } else if old_value == 0 {
            self.set_flag(Flag::Status(StatusFlag::Overflow), true);
            u16::MAX
        } else {
            self.set_flag(Flag::Status(StatusFlag::Overflow), false);
            old_value - 1
        };

        self.set_flags_u16(new_value);
        if instruction.is_inc {
            self.set_flag(Flag::Status(StatusFlag::AuxiliaryCarry), old_value == 15);
        } else {
            self.set_flag(Flag::Status(StatusFlag::AuxiliaryCarry), old_value == 16);
        }

        self.set_register(&instruction.target, new_value);
    }

    #[allow(dead_code)]
    fn step_ret(&mut self) {
        let sp = Register::Special(SpecialRegister::StackPointer);
        let sp_prev = self.get_register(&sp);
        let new_sp = if sp_prev > u16::MAX - 2 {
            sp_prev - (u16::MAX - 2)
        } else {
            sp_prev + 2
        };
        self.set_register(&sp, new_sp);
        let (new_counter, _) = self.get_memory_word(sp_prev as usize);
        self.program_counter = new_counter;
    }

    /// Returns the number of extra clock cycles incurred by misaligned memory access.
    fn step_logic(&mut self, instruction: &LogicInstruction) -> u8 {
        if instruction.amount_from_cl {
            todo!()
        }
        match &instruction.op {
            LogicInstructionType::Shr => match &instruction.target {
                LogicTarget::Register(reg) => {
                    if instruction.is_wide {
                        let current_value = self.get_register(reg);
                        let new_value = current_value >> 1;
                        self.set_register(reg, new_value);
                        if (current_value > 1 << 15) == (new_value > 1 << 15) {
                            self.set_flag(Flag::Status(StatusFlag::Overflow), false)
                        } else {
                            self.set_flag(Flag::Status(StatusFlag::Overflow), true)
                        }
                        self.set_flags_u16(new_value);
                        self.set_flag(Flag::Status(StatusFlag::Carry), current_value % 2 == 1);
                    } else {
                        let current_value = self.get_register(reg) as u8;
                        let new_value = current_value >> 1;
                        self.set_register(reg, new_value as u16);
                        if (current_value > 1 << 7) == (new_value > 1 << 7) {
                            self.set_flag(Flag::Status(StatusFlag::Overflow), false)
                        } else {
                            self.set_flag(Flag::Status(StatusFlag::Overflow), true)
                        }
                        self.set_flags_u8(new_value);
                        self.set_flag(Flag::Status(StatusFlag::Carry), current_value % 2 == 1);
                    }

                    0
                }
                LogicTarget::Address(addr) => {
                    if instruction.is_wide {
                        let addr = self.resolve_eaddr(addr);
                        let (current_value, slow) = self.get_memory_word(addr);
                        let new_value = current_value >> 1;
                        let slow_2 = self.set_memory_word(addr, new_value);
                        if (current_value > 1 << 15) == (new_value > 1 << 15) {
                            self.set_flag(Flag::Status(StatusFlag::Overflow), false)
                        } else {
                            self.set_flag(Flag::Status(StatusFlag::Overflow), true)
                        }
                        self.set_flags_u16(new_value);
                        self.set_flag(Flag::Status(StatusFlag::Carry), current_value % 2 == 1);
                        (if slow { 4 } else { 0 }) + if slow_2 { 4 } else { 0 }
                    } else {
                        let addr = self.resolve_eaddr(addr);
                        let current_value = self.get_memory_byte(addr);
                        let new_value = current_value >> 1;
                        self.set_memory_byte(addr, new_value);
                        if (current_value > 1 << 7) == (new_value > 1 << 7) {
                            self.set_flag(Flag::Status(StatusFlag::Overflow), false)
                        } else {
                            self.set_flag(Flag::Status(StatusFlag::Overflow), true)
                        }
                        self.set_flags_u8(new_value);
                        self.set_flag(Flag::Status(StatusFlag::Carry), current_value % 2 == 1);
                        0
                    }
                }
            },
            _ => todo!(),
        }
    }

    /// Returns a string description of the jump, and a bool indicating
    /// whether the jump actually happened.
    fn step_jump(&mut self, jump: Jump, offset: i8) -> (String, bool) {
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
                if prev == 0 {
                    self.set_register(&reg, u16::MAX);
                } else {
                    self.set_register(&reg, prev - 1);
                };
                prev != 1
            }
            Jump::Loopz => {
                let reg = Register::General(GeneralRegister::C, RegisterSubset::All);
                let prev = self.get_register(&reg);
                self.set_register(&reg, prev - 1);
                prev != 1 && self.flags.get(Flag::Status(StatusFlag::Zero))
            }
            Jump::Loopnz => {
                let reg = Register::General(GeneralRegister::C, RegisterSubset::All);
                let prev = self.get_register(&reg);
                self.set_register(&reg, prev - 1);
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
        (
            format!(
                "{} ${}{}",
                jump,
                if offset > 0 { "+" } else { "" },
                offset + 2,
            ),
            should_jump,
        )
    }

    pub fn get_program_counter(&self) -> u16 {
        self.program_counter
    }

    /// Returns a string representation of what happened, and true if we are meant to stop.
    pub fn step(
        &mut self,
        instruction: &Instruction<i8>,
        display_ip: bool,
        show_clock: bool,
    ) -> (String, bool) {
        let advance = instruction.length();
        let old_ip = if display_ip {
            Some(self.program_counter)
        } else {
            None
        };
        let old_flags = self.flags;
        let old_registers = self.registers.clone();

        self.program_counter += advance as u16;

        let (instruction_override, stop, slowness) = match instruction {
            Instruction::Move(mov) => {
                let slow = self.step_mov(mov);
                (None, false, if slow { 4 } else { 0 })
            }
            Instruction::Arithmetic(arith) => {
                let slowness = self.step_arithmetic(arith);
                (None, false, slowness)
            }
            Instruction::Jump(jump, offset) => {
                let overridden = self.step_jump(*jump, *offset);
                (Some(overridden), false, 0)
            }
            Instruction::Boolean(boolean) => {
                self.step_boolean(boolean);
                (None, false, 0)
            }
            Instruction::Inc(inc) => {
                self.step_inc(inc);
                (None, false, 0)
            }
            Instruction::Ret => (None, true, 0),
            Instruction::Logic(instruction) => {
                self.step_logic(instruction);
                (None, false, 0)
            }
            Instruction::Trivia(_) => (None, false, 0),
        };

        if stop {
            self.program_counter -= 1; // compensate for pre-incrementing
            (
                format!(
                    "STOPONRET: Return encountered at address {}.",
                    self.program_counter
                ),
                true,
            )
        } else {
            let mut post: Vec<String> = Vec::new();

            let (clock_count, clock_description) = match &instruction_override {
                None => instruction.clock_count(None),
                Some((_, jumped)) => instruction.clock_count(Some(!*jumped)),
            };
            let new_clock = self.clocks_executed + clock_count + slowness as u32;
            if show_clock {
                post.push("Clocks:".to_owned());
                post.push(format!(
                    "+{} = {}",
                    new_clock - self.clocks_executed,
                    new_clock
                ));
                if !clock_description.is_empty() {
                    post.push(format!(
                        "({}{})",
                        clock_description,
                        if slowness > 0 {
                            format!(" + {slowness}p")
                        } else {
                            "".to_owned()
                        }
                    ))
                }
                post.push("|".to_owned());
            }
            self.clocks_executed = new_clock;

            let acc_desc = self.registers.diff(&old_registers);
            if !acc_desc.is_empty() {
                post.push(acc_desc);
            }

            match old_ip {
                None => {}
                Some(old_ip) => {
                    post.push(format!(
                        "ip:{}->{}",
                        display_small(old_ip),
                        display_small(self.program_counter)
                    ));
                }
            }

            if old_flags != self.flags {
                post.push(format!("flags:{}->{}", old_flags, self.flags));
            }

            let post = post.join(" ");

            let instruction = match instruction_override {
                None => format!("{instruction}"),
                Some((i, _)) => i,
            };

            if post.is_empty() {
                (instruction, false)
            } else {
                (format!("{instruction} ; {post}"), false)
            }
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
            if value != 0 {
                result.push_str(&format!("{}x: {} ({})\n", r, display_big(value), value))
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
                result.push_str(&format!("{}: {} ({})\n", r, display_big(value), value))
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
                result.push_str(&format!("{}: {} ({})\n", r, display_big(value), value));
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
