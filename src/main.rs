fn main() -> anyhow::Result<()> {
    formality_core::with_tracing_logs(formality::main)
}
