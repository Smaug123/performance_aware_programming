#[cfg(test)]
mod test_computer {
    use sim_8086::{computer::Computer, program::Program};
    use std::str;

    fn clean_trace(s: &str) -> String {
        let mut s = s
            .replace("\r\n", "\n")
            .replace("      ", "")
            .replace(" \n", "\n");
        let first_newline = s.find('\n').unwrap();
        s.drain(0..=first_newline);
        s
    }

    fn test_sim<T>(input_bytecode: T, expected_trace: &str, display_ip: bool)
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
                    trace.push(computer.step(instruction, display_ip));
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

        assert_eq!(
            trace,
            clean_trace(expected_trace).lines().collect::<Vec<_>>()
        )
    }

    #[test]
    fn test_immediate_movs() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs.txt");
        test_sim(input_bytecode, expected_trace, false)
    }

    #[test]
    fn test_register_movs() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0044_register_movs");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0044_register_movs.txt");
        test_sim(input_bytecode, expected_trace, false)
    }

    #[test]
    fn test_challenge_register_movs() {
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs"
        );
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs.txt"
        );
        test_sim(input_bytecode, expected_trace, false)
    }

    #[test]
    fn test_add_sub_cmp() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp.txt");
        test_sim(input_bytecode, expected_trace, false)
    }

    #[test]
    fn test_challenge_flags() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags.txt");
        test_sim(input_bytecode, expected_trace, false)
    }

    #[test]
    fn test_ip_register() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0048_ip_register");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0048_ip_register.txt");
        test_sim(input_bytecode, expected_trace, true)
    }

    /*
    #[test]
    fn test_conditional_jumps() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0049_conditional_jumps");
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0049_conditional_jumps.txt"
        );
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_challenge_jumps() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0050_challenge_jumps");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0050_challenge_jumps.txt");
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_memory_mov() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0051_memory_mov");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0051_memory_mov.txt");
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_memory_add_loop() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0052_memory_add_loop");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0052_memory_add_loop.txt");
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_add_loop_challenge() {
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0053_add_loop_challenge"
        );
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0053_add_loop_challenge.txt"
        );
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_draw_rectangle() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0054_draw_rectangle");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0054_draw_rectangle.txt");
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_draw_rectangle_challenge() {
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0055_challenge_rectangle"
        );
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0055_challenge_rectangle.txt"
        );
        test_sim(input_bytecode, expected_trace)
    }
    */
}
