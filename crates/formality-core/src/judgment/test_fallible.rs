#![cfg(test)]

//! Test that we permit the use of `?` in `let` and `if let`

use anyhow::bail;

use crate::cast_impl;
use crate::judgment_fn;
use crate::Fallible;

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug, Hash)]
struct Check {
    x: u32,
}

cast_impl!(Check);

impl Check {
    fn is(&self, x: u32) -> Fallible<u32> {
        if self.x == x {
            Ok(x)
        } else {
            bail!("expected {} got {}", self.x, x)
        }
    }
}

judgment_fn!(
    fn jfn(c: Check, x: u32) => u32 {
        debug(c, x)
        assert(x < 100)
        assert(check_x(x))

        (
            (let y = c.is(x)?)
            --------------------------------------- ("rule")
            (jfn(c, x) => y)
        )


        (
            (if let 44 = c.is(x)?)
            --------------------------------------- ("other-rule")
            (jfn(c, x) => 45)
        )
    }
);

fn check_x(x: u32) -> Fallible<()> {
    if x > 75 {
        bail!("invalid x: {x}")
    }
    Ok(())
}

#[test]
fn is_equal_22() {
    jfn(Check { x: 22 }, 22).assert_ok(expect_test::expect![[r#"
        {
          22,
        }
    "#]]);
}

#[test]
fn is_equal_44() {
    jfn(Check { x: 44 }, 44).assert_ok(expect_test::expect![[r#"
        {
          44,
          45,
        }
    "#]]);
}

#[test]
fn is_not_equal() {
    jfn(Check { x: 22 }, 23).assert_err(expect_test::expect![[r#"
        judgment `jfn { c: Check { x: 22 }, x: 23 }` failed at the following rule(s):
          the rule "other-rule" failed at step #0 (src/file.rs:LL:CC) because
            expected 22 got 23
          the rule "rule" failed at step #0 (src/file.rs:LL:CC) because
            expected 22 got 23"#]]);
}

#[test]
#[should_panic(expected = "judgment assertion failed: `x < 100` is false")]
fn bool_assertion_fails() {
    let _ = jfn(Check { x: 22 }, 110);
}

#[test]
#[should_panic(expected = "judgment assertion failed: `check_x(x)` got invalid x")]
fn result_assertion_fails() {
    let _ = jfn(Check { x: 22 }, 76);
}
