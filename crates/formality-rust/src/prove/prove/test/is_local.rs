use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Program;

use crate::prove::prove::test_util::test_prove;

#[test]
fn test_forall_not_local() {
    test_prove(Program::empty(), term("{} => {for<T> @IsLocal(Debug(T))}")).assert_err(expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: !ty_1, assumptions: {}, env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "local trait" at (is_local.rs) failed because
          condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
            decls = program([], 222)
            &goal.trait_id = Debug"#]]);
}

#[test]
fn test_exists_not_local() {
    test_prove(
        Program::empty(),
        term("exists<T> {} => {@IsLocal(Debug(T))}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: false, substitution: {} }}"])
}
