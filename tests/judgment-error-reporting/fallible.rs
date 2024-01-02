use anyhow::Context;
use formality_core::{judgment_fn, term, test, test_util::ResultTestExt, Fallible};

#[term]
enum Ty {
    Class { name: ClassName },
}

formality_core::id!(ClassName);

judgment_fn! {
    fn sub(
        a: Ty,
        b: Ty,
    ) => () {
        debug(a, b)

        (
            (if name_a == name_b)
            ---------------------- ("same class")
            (sub(Ty::Class { name: name_a }, Ty::Class { name: name_b }) => ())
        )
    }
}

fn check_sub(a: Ty, b: Ty) -> Fallible<()> {
    Ok(sub(a, b)
        .check_proven()
        .with_context(|| format!("check_sub"))?)
}

#[test]
fn test() {
    let foo = Ty::Class {
        name: ClassName::new("Foo"),
    };
    let bar = Ty::Class {
        name: ClassName::new("Bar"),
    };
    // Demonstrates a multi-line error rendered by anyhow.
    check_sub(foo, bar).assert_err(expect_test::expect![[r#"
        check_sub

        Caused by:
            judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
              the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
                condition evaluted to false: `name_a == name_b`
    "#]]);
}
