#[cfg(test)]
mod test_computer {
    use sim_8086::{computer::Computer, program::Program};
    use std::str;

    fn clean_trace(s: &str) -> String {
        let mut s = s
            .replace("\r\n", "\n")
            .replace("      ", "")
            .replace(" \n", "\n");

        // Chop the initial header on trace reports
        if let Some(index) = s.find(" ---") {
            s.drain(0..=index);
        }
        let first_newline = s.find('\n').unwrap();
        s.drain(0..=first_newline);

        // Chop the 8088 section on trace reports
        if let Some(index) = s.find("\n******") {
            s.drain(index..);
        }
        s
    }

    fn test_sim<T>(input_bytecode: T, expected_trace: &str, display_ip: bool, show_clock: bool)
    where
        T: AsRef<[u8]>,
    {
        let mut computer = Computer::new();

        let decoded = Program::of_bytes(input_bytecode.as_ref().iter().cloned());

        let mut instructions = vec![None; input_bytecode.as_ref().len()];
        let mut counter = 0usize;
        for instruction in decoded.instructions.iter() {
            instructions[counter] = Some(instruction);
            counter += instruction.length() as usize;
        }

        let end_of_instructions = counter;
        let mut trace: Vec<String> = vec![];

        loop {
            let counter = computer.get_program_counter() as usize;
            if counter >= end_of_instructions {
                break;
            }
            match instructions[counter] {
                None => {
                    panic!("landed in middle of instruction")
                }
                Some(instruction) => {
                    trace.push(computer.step(instruction, display_ip, show_clock));
                }
            }
        }

        trace.push("".to_owned());
        trace.push("Final registers:".to_owned());
        for line in computer.dump_register_state().lines() {
            trace.push(line.to_string());
        }

        if display_ip {
            let ip = computer.get_program_counter();
            trace.push(format!("ip: {:#06x} ({})", ip, ip));
        }

        let flags = computer.dump_flag_state();
        if !flags.is_empty() {
            trace.push(format!("   flags: {}", flags));
        }

        trace.push("".to_owned());

        let cleaned = clean_trace(expected_trace);
        let expected = cleaned.lines().collect::<Vec<_>>();

        assert_eq!(trace.len(), expected.len());

        for (traced, expected) in trace.iter().zip(expected.iter()) {
            assert_eq!(traced, expected)
        }
    }

    #[test]
    fn test_immediate_movs() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs.txt");
        test_sim(input_bytecode, expected_trace, false, false)
    }

    #[test]
    fn test_register_movs() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0044_register_movs");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0044_register_movs.txt");
        test_sim(input_bytecode, expected_trace, false, false)
    }

    #[test]
    fn test_challenge_register_movs() {
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs"
        );
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs.txt"
        );
        test_sim(input_bytecode, expected_trace, false, false)
    }

    #[test]
    fn test_add_sub_cmp() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp.txt");
        test_sim(input_bytecode, expected_trace, false, false)
    }

    #[test]
    fn test_challenge_flags() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags.txt");
        test_sim(input_bytecode, expected_trace, false, false)
    }

    #[test]
    fn test_ip_register() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0048_ip_register");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0048_ip_register.txt");
        test_sim(input_bytecode, expected_trace, true, false)
    }

    #[test]
    fn test_conditional_jumps() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0049_conditional_jumps");
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0049_conditional_jumps.txt"
        );
        test_sim(input_bytecode, expected_trace, true, false)
    }

    #[test]
    fn test_challenge_jumps() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0050_challenge_jumps");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0050_challenge_jumps.txt");
        test_sim(input_bytecode, expected_trace, true, false)
    }

    #[test]
    fn test_memory_mov() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0051_memory_mov");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0051_memory_mov.txt");
        test_sim(input_bytecode, expected_trace, true, false)
    }

    #[test]
    fn test_memory_add_loop() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0052_memory_add_loop");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0052_memory_add_loop.txt");
        test_sim(input_bytecode, expected_trace, true, false)
    }

    #[test]
    fn test_add_loop_challenge() {
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0053_add_loop_challenge"
        );
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0053_add_loop_challenge.txt"
        );
        test_sim(input_bytecode, expected_trace, true, false)
    }

    #[test]
    fn test_draw_rectangle() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0054_draw_rectangle");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0054_draw_rectangle.txt");
        test_sim(input_bytecode, expected_trace, true, false)
    }

    #[test]
    fn test_draw_rectangle_challenge() {
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0055_challenge_rectangle"
        );
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0055_challenge_rectangle.txt"
        );
        test_sim(input_bytecode, expected_trace, true, false)
    }

    #[test]
    fn test_estimating_cycles() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0056_estimating_cycles");
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0056_estimating_cycles.txt"
        );
        test_sim(input_bytecode, expected_trace, true, true)
    }

    #[test]
    fn test_challenge_cycles() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0057_challenge_cycles");
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0057_challenge_cycles.txt"
        );
        test_sim(input_bytecode, expected_trace, true, true)
    }

    #[test]
    fn test_single_scalar() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0059_SingleScalar");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0059_SingleScalar.txt");
        test_sim(input_bytecode, expected_trace, true, true)
    }

    #[test]
    fn test_unroll2_scalar() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0060_Unroll2Scalar");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0060_Unroll2Scalar.txt");
        test_sim(input_bytecode, expected_trace, true, true)
    }

    #[test]
    fn test_dual_scalar() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0061_DualScalar");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0061_DualScalar.txt");
        test_sim(input_bytecode, expected_trace, true, true)
    }

    #[test]
    fn test_quad_scalar() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0062_QuadScalar");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0062_QuadScalar.txt");
        test_sim(input_bytecode, expected_trace, true, true)
    }

    #[test]
    fn test_quad_scalar_ptr() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0063_QuadScalarPtr");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0063_QuadScalarPtr.txt");
        test_sim(input_bytecode, expected_trace, true, true)
    }

    #[test]
    fn test_tree_scalar_ptr() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0064_TreeScalarPtr");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0064_TreeScalarPtr.txt");
        test_sim(input_bytecode, expected_trace, true, true)
    }
}
