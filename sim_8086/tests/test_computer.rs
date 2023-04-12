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
}
