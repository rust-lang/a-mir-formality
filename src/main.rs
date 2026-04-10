fn main() -> anyhow::Result<()> {
    formality_core::with_tracing_logs(a_mir_formality::main)
}
