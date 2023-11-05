use formality_core::term;
use std::sync::Arc;

#[test]
#[should_panic(expected = "ambiguous parse")] // FIXME: we want this example to work
fn reduce_reduce_ok() {
    #[term]
    pub enum Root {
        #[cast]
        ClassTy(ClassTy),
        #[cast]
        Perm(Perm),
    }

    #[term($perm $class_id)]
    pub struct ClassTy {
        perm: Perm,
        class_id: Id,
    }

    #[term]
    pub enum Perm {
        My,
        Our,
    }

    formality_core::id!(Id);

    let term: Root = crate::ptt::term("my String");
    expect_test::expect![].assert_debug_eq(&term);
}

#[test]
#[should_panic(expected = "ambiguous parse")]
fn reduce_reduce_ambig() {
    #[term]
    pub enum Root {
        #[grammar($v0)]
        OneId(Id),
        #[grammar($v0 $v1)]
        TwoId(Id, Id),
        #[grammar($v0 $v1)]
        TwoRr(Arc<Root>, Arc<Root>),
    }

    formality_core::id!(Id);

    // This will panic. It could be parsed in multiple ways
    // (using a variant of Reverse Polish Notation) and none is obviously
    // better than the other:
    //
    // Root = ((Id Root::OneId) (Id Id Root::TwoId) Root::TwoRr)
    // Root = (Id Id Root::TwoId) (Id Root::OneId) Root::TwoRr)
    // Root = ((Id Root::OneId) (Id Root::OneId) (Id Root::OneId) Root::TwoRr)
    let term: Root = crate::ptt::term("a b c");
    expect_test::expect![].assert_debug_eq(&term);
}
