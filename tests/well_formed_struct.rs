use a_mir_formality::{crates, FormalityTest};

#[test]
fn struct_with_lifetime() {
    FormalityTest::new(crates![crate test {
        struct Foo<'a> {
            y: &'a u32,
        }

        fn main() -> () {
            exists<'y> {
                let a: u32 = 22 _ u32;
                let f: Foo<'y> = Foo::<'y> { y: &'y a };
            }
        }
    }])
    .err(expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ wf(Foo<?lt_0>), via: @ wf(?lt_0), assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove/prove/prove_wf.rs:14:1: no applicable rules for prove_wf { goal: ?lt_0, assumptions: {@ wf(?lt_0)}, env: Env { variables: [?lt_0], bias: Soundness, pending: [], allow_pending_outlives: true } }"#
    ]])
}
