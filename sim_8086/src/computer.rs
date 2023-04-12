use crate::{
    instruction::Instruction,
    move_instruction::{ImmediateToRegister, ImmediateToRegisterOrMemory, MoveInstruction},
    register::{ByteRegisterSubset, GeneralRegister, Register, RegisterSubset, SpecialRegister},
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
        let was = self.get_register(r);
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
        let is_now = self.get_register(r);
        format!(
            "{}:{}->{}",
            r,
            Self::display_small(was),
            Self::display_small(is_now)
        )
    }

    fn step_mov(&mut self, instruction: &MoveInstruction) -> String {
        let preamble = format!("{}", instruction);
        let description = match &instruction {
            MoveInstruction::RegRegMove(_) => todo!(),
            MoveInstruction::RegMemMove(_) => todo!(),
            MoveInstruction::MemRegMove(_) => todo!(),
            MoveInstruction::ImmediateToRegister(mov) => match mov {
                ImmediateToRegister::Byte(dest, value) => self.set_register(dest, *value as u16),
                ImmediateToRegister::Wide(dest, value) => self.set_register(dest, *value),
            },
            MoveInstruction::ImmediateToRegisterOrMemory(mov) => match mov {
                ImmediateToRegisterOrMemory::Byte(_, _) => todo!(),
                ImmediateToRegisterOrMemory::Word(_, _) => todo!(),
            },
            MoveInstruction::MemoryToAccumulator(_) => todo!(),
            MoveInstruction::AccumulatorToMemory(_) => todo!(),
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

        result
    }
}
