//! `formality-core` contains core definitions that can be used for
//! languages that are not Rust. It is intended to play a role similar
//! to
//!

pub use tracing::debug;
pub use tracing::instrument;
pub use tracing::trace;

pub mod binder;
pub mod cast;
pub mod collections;
pub mod fold;
pub mod language;
pub mod parse;
pub mod variable;
pub mod visit;

/// Run an action with a tracing log subscriber. The logging level is loaded
/// from `RUST_LOG`. The `formality_macro::test` expansion uses this to enable logs.
pub fn with_tracing_logs<T>(action: impl FnOnce() -> T) -> T {
    use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};
    use tracing_tree::HierarchicalLayer;
    let filter = EnvFilter::from_env("RUST_LOG");
    let subscriber = Registry::default()
        .with(filter)
        .with(HierarchicalLayer::new(2).with_writer(std::io::stdout));
    tracing::subscriber::with_default(subscriber, action)
}
