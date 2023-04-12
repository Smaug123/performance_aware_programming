#[cfg(test)]
mod test_program {
    use std::{
        collections::{HashMap, HashSet},
        marker::PhantomData,
    };

    use sim_8086::{
        arithmetic_instruction::{
            ArithmeticInstruction, ArithmeticInstructionSelect, ArithmeticOperation,
        },
        assembly,
        instruction::Instruction,
        program::Program,
        register::{GeneralRegister, Register, RegisterSubset},
    };

    fn instruction_equal_ignoring_labels<A, B>(i1: &Instruction<A>, i2: &Instruction<B>) -> bool {
        match (i1, i2) {
            (Instruction::Move(i1), Instruction::Move(i2)) => i1 == i2,
            (Instruction::Move(_), _) => false,
            (Instruction::Arithmetic(i1), Instruction::Arithmetic(i2)) => i1 == i2,
            (Instruction::Arithmetic(_), _) => false,
            (Instruction::Jump(i1, _), Instruction::Jump(i2, _)) => i1 == i2,
            (Instruction::Jump(_, _), _) => false,
            (Instruction::Trivia(_), Instruction::Trivia(_)) => true,
            (Instruction::Trivia(_), _) => false,
        }
    }

    fn test_parser_lax<T>(
        input_asm: &str,
        input_bytecode: T,
        permit_equivalences: HashMap<Instruction<&str>, Instruction<&str>>,
    ) where
        T: AsRef<[u8]>,
    {
        let (remaining, parsed) = assembly::program(input_asm).unwrap();
        assert_eq!(remaining, "");
        assert_eq!(parsed.bits, 16);

        let adjusted_program: Program<Vec<Instruction<_>>, _> = Program {
            bits: parsed.bits,
            instructions: parsed
                .instructions
                .into_iter()
                .map(|i| match permit_equivalences.get(&i) {
                    Some(v) => v.clone(),
                    None => i.clone(),
                })
                .collect(),
            offset: PhantomData,
        };

        for (i, (actual, expected)) in adjusted_program
            .to_bytes()
            .iter()
            .zip(input_bytecode.as_ref().iter())
            .enumerate()
        {
            if actual != expected {
                panic!(
                    "Failed assertion: expected {} (from Casey), got {}, at position {}\n{:?}",
                    expected,
                    actual,
                    i,
                    adjusted_program.to_bytes()
                )
            }
        }
    }

    fn test_parser<T>(input_asm: &str, input_bytecode: T)
    where
        T: AsRef<[u8]>,
    {
        test_parser_lax(input_asm, input_bytecode, HashMap::new())
    }

    fn test_disassembler_lax<T>(
        input_asm: &str,
        input_bytecode: T,
        permit_equivalences: HashSet<(Vec<u8>, Vec<u8>)>,
    ) where
        T: AsRef<[u8]>,
    {
        let disassembled = Program::of_bytes(input_bytecode.as_ref().iter().cloned());

        let (remaining, pre_compiled) = assembly::program(&input_asm).unwrap();
        assert_eq!(remaining, "");

        let disassembled = disassembled.instructions.iter().filter(|i| match i {
            Instruction::Trivia(_) => false,
            _ => true,
        });
        let mut compiled = pre_compiled.instructions.iter().filter(|i| match i {
            Instruction::Trivia(_) => false,
            _ => true,
        });

        let mut is_different = false;

        for dis in disassembled {
            if let Some(compiled) = compiled.next() {
                if !instruction_equal_ignoring_labels(dis, compiled) {
                    let compiled_bytes = compiled.to_bytes();
                    let dis_bytes = dis.to_bytes();
                    if !permit_equivalences.contains(&(compiled_bytes.clone(), dis_bytes.clone()))
                        && !permit_equivalences
                            .contains(&(dis_bytes.clone(), compiled_bytes.clone()))
                    {
                        println!(
                            "Different instruction. From disassembly: {dis} ({:?}). From our compilation: {compiled} ({:?}).",
                            compiled_bytes,
                            dis_bytes
                        );
                        is_different = true;
                    }
                }
            } else {
                println!(
                    "Extra instruction from disassembly: {dis} ({:?})",
                    dis.to_bytes()
                );
                is_different = true;
            }
        }

        while let Some(compiled) = compiled.next() {
            println!(
                "Extra instruction from compilation: {compiled} ({:?})",
                compiled.to_bytes()
            );
            is_different = true;
        }

        if is_different {
            panic!("Disassembling input bytecode produced a different program from compiling the input asm.")
        }
    }

    fn test_disassembler<T>(input_asm: &str, input_bytecode: T)
    where
        T: AsRef<[u8]>,
    {
        test_disassembler_lax(input_asm, input_bytecode, HashSet::new())
    }

    #[test]
    fn test_register_register_mov_parser() {
        let input_asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0037_single_register_mov.asm"
        );
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0037_single_register_mov"
        );
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_register_mov_disassembler() {
        let bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0037_single_register_mov"
        );
        let asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0037_single_register_mov.asm"
        );
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_register_register_many_mov_parser() {
        let input_asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0038_many_register_mov.asm"
        );
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0038_many_register_mov");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_register_many_mov_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0038_many_register_mov");
        let asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0038_many_register_mov.asm"
        );
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_register_more_mov_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0039_more_movs.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0039_more_movs");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_more_mov_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0039_more_movs");
        let asm = include_str!("../../computer_enhance/perfaware/part1/listing_0039_more_movs.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_register_challenge_movs_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0040_challenge_movs.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0040_challenge_movs");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_challenge_movs_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0040_challenge_movs");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0040_challenge_movs.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_add_sub_cmp_jnz_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_add_sub_cmp_jnz_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0041_add_sub_cmp_jnz.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_immediate_movs_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_immediate_movs_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_register_movs_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0044_register_movs.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0044_register_movs");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_register_movs_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0044_register_movs");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0044_register_movs.asm");
        test_disassembler(asm, bytecode)
    }

    /*
    We have not yet implemented the segment registers, so this test can't pass.
        #[test]
        fn test_challenge_register_movs_parser() {
            let input_asm = include_str!(
                "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs.asm"
            );
            let input_bytecode = include_bytes!(
                "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs"
            );
            test_parser(input_asm, input_bytecode)
        }

        #[test]
        fn test_challenge_register_movs_disassembler() {
            let bytecode = include_bytes!(
                "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs"
            );
            let asm = include_str!(
                "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs.asm"
            );
            test_disassembler(asm, bytecode)
        }
    */

    #[test]
    fn test_add_sub_cmp_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_add_sub_cmp_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_challenge_flags_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_challenge_flags_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_ip_register_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0048_ip_register.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0048_ip_register");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_ip_register_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0048_ip_register");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0048_ip_register.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_conditional_jumps_parser() {
        let input_asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0049_conditional_jumps.asm"
        );
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0049_conditional_jumps");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_conditional_jumps_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0049_conditional_jumps");
        let asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0049_conditional_jumps.asm"
        );
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_challenge_jumps_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0050_challenge_jumps.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0050_challenge_jumps");
        let mut swaps = HashMap::new();
        swaps.insert(
            Instruction::Arithmetic(ArithmeticInstruction {
                op: ArithmeticOperation::Add,
                instruction: ArithmeticInstructionSelect::ImmediateToAccWord(1),
            }),
            Instruction::Arithmetic(ArithmeticInstruction {
                op: ArithmeticOperation::Add,
                instruction: ArithmeticInstructionSelect::ImmediateToRegisterWord(
                    Register::General(GeneralRegister::A, RegisterSubset::All),
                    1,
                    true,
                ),
            }),
        );
        test_parser_lax(input_asm, input_bytecode, swaps)
    }

    #[test]
    fn test_challenge_jumps_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0050_challenge_jumps");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0050_challenge_jumps.asm");
        let mut allowed = HashSet::new();
        // We implemented `add ax, 1` using "immediate to accumulator";
        // in this example, Casey implemented it using "immediate to register".
        allowed.insert((vec![5, 1, 0], vec![131, 192, 1]));
        test_disassembler_lax(asm, bytecode, allowed)
    }

    #[test]
    fn test_memory_mov_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0051_memory_mov.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0051_memory_mov");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_memory_mov_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0051_memory_mov");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0051_memory_mov.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_memory_add_loop_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0052_memory_add_loop.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0052_memory_add_loop");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_memory_add_loop_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0052_memory_add_loop");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0052_memory_add_loop.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_add_loop_challenge_parser() {
        let input_asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0053_add_loop_challenge.asm"
        );
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0053_add_loop_challenge"
        );
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_add_loop_challenge_disassembler() {
        let bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0053_add_loop_challenge"
        );
        let asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0053_add_loop_challenge.asm"
        );
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_draw_rectangle_parser() {
        let input_asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0054_draw_rectangle.asm");
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0054_draw_rectangle");
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_draw_rectangle_disassembler() {
        let bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0054_draw_rectangle");
        let asm =
            include_str!("../../computer_enhance/perfaware/part1/listing_0054_draw_rectangle.asm");
        test_disassembler(asm, bytecode)
    }

    #[test]
    fn test_challenge_rectangle_parser() {
        let input_asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0055_challenge_rectangle.asm"
        );
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0055_challenge_rectangle"
        );
        test_parser(input_asm, input_bytecode)
    }

    #[test]
    fn test_challenge_rectangle_disassembler() {
        let bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0055_challenge_rectangle"
        );
        let asm = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0055_challenge_rectangle.asm"
        );
        test_disassembler(asm, bytecode)
    }
}
