#[derive(Eq, PartialEq, Debug, Hash, Clone)]
pub enum TriviaInstruction<InstructionOffset> {
    Label(InstructionOffset),
}
