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

    fn test_sim<T>(input_bytecode: T, expected_trace: &str)
    where
        T: AsRef<[u8]>,
    {
        let mut computer = Computer::new();

        let decoded = Program::of_bytes(input_bytecode.as_ref().iter().cloned());

        let mut trace = "".to_owned();

        for instruction in decoded.instructions {
            trace.push_str(&computer.step(&instruction));
            trace.push('\n');
        }

        trace.push_str("\nFinal registers:\n");
        trace.push_str(&computer.dump_register_state());

        trace.push_str("\n");

        assert_eq!(trace, clean_trace(expected_trace))
    }

    #[test]
    fn test_immediate_movs() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0043_immediate_movs.txt");
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_register_movs() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0044_register_movs");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0044_register_movs.txt");
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_challenge_register_movs() {
        let input_bytecode = include_bytes!(
            "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs"
        );
        let expected_trace = include_str!(
            "../../computer_enhance/perfaware/part1/listing_0045_challenge_register_movs.txt"
        );
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_add_sub_cmp() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0046_add_sub_cmp.txt");
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_challenge_flags() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0047_challenge_flags.txt");
        test_sim(input_bytecode, expected_trace)
    }

    #[test]
    fn test_ip_register() {
        let input_bytecode =
            include_bytes!("../../computer_enhance/perfaware/part1/listing_0048_ip_register");
        let expected_trace =
            include_str!("../../computer_enhance/perfaware/part1/listing_0048_ip_register.txt");
        test_sim(input_bytecode, expected_trace)
    }

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
}
