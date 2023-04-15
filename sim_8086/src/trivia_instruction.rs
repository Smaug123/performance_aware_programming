use arbitrary::Arbitrary;

#[derive(Eq, PartialEq, Debug, Hash, Clone, Arbitrary)]
pub enum TriviaInstruction<InstructionOffset> {
    Label(InstructionOffset),
}
