use formality_core::{term, test};
use std::sync::Arc;

// ANCHOR: path
#[term]
pub enum Path {
    #[cast]
    Id(Id),

    #[grammar($v0 . $v1)]
    Field(Arc<Path>, Id),

    #[grammar($v0 [ $v1 ])]
    Index(Arc<Path>, Arc<Path>),
}

formality_core::id!(Id);
// ANCHOR_END: path

#[test]
fn path() {
    let term: Path = crate::ptt::term("a.b[c.d].e");
    expect_test::expect![[r#"
        Field(
            Index(
                Field(
                    Id(
                        a,
                    ),
                    b,
                ),
                Field(
                    Id(
                        c,
                    ),
                    d,
                ),
            ),
            e,
        )
    "#]]
    .assert_debug_eq(&term);
}
