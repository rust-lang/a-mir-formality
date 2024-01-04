use std::sync::Arc;

use formality_core::{judgment_fn, term, test};

#[term]
enum Ty {
    Class { name: ClassName },
    My(Arc<Ty>),
}

formality_core::id!(ClassName);

judgment_fn! {
    fn sub(
        a: Ty,
        b: Ty,
    ) => () {
        debug(a, b)

        (
            (equivalent(&a) => a1)
            (equivalent(&b) => b1)
            (if a1 != a || b1 != b)!
            (sub(&a1, b1) => ())
            ---------------------- ("equivalent")
            (sub(a: Ty, b: Ty) => ())
        )

        (
            (if name_a == name_b)
            ---------------------- ("same class")
            (sub(Ty::Class { name: name_a }, Ty::Class { name: name_b }) => ())
        )

        (
            (sub(&*a, &*b) => ())
            ---------------------- ("both my")
            (sub(Ty::My(a), Ty::My(b)) => ())
        )
    }
}

judgment_fn! {
    fn equivalent(a: Ty) => Ty {
        debug(a)

        (
            ----------------------------- ("identity")
            (equivalent(a) => a)
        )

        (
            ----------------------------- ("add my")
            (equivalent(Ty::Class { name }) => Ty::My(Arc::new(Ty::Class { name })))
        )


        (
            ----------------------------- ("strip my")
            (equivalent(Ty::My(t)) => &*t)
        )
    }
}

#[test]
fn test() {
    let foo = Ty::Class {
        name: ClassName::new("Foo"),
    };
    let bar = Ty::Class {
        name: ClassName::new("Bar"),
    };
    sub(foo, bar).assert_err(
      expect_test::expect![[r#"
          judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
            the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
              condition evaluted to false: `name_a == name_b`
                name_a = Foo
                name_b = Bar"#]]
    );
}

#[test]
fn test1() {
    let foo = Ty::My(Arc::new(Ty::Class {
        name: ClassName::new("Foo"),
    }));
    let bar = Ty::Class {
        name: ClassName::new("Bar"),
    };
    sub(foo, bar).assert_err(
      expect_test::expect![[r#"
          judgment `sub { a: my(class(Foo)), b: class(Bar) }` failed at the following rule(s):
            the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
              judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
                the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
                  condition evaluted to false: `name_a == name_b`
                    name_a = Foo
                    name_b = Bar
            the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
              judgment `sub { a: class(Foo), b: my(class(Bar)) }` failed at the following rule(s):
                the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
                  judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
                    the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
                      condition evaluted to false: `name_a == name_b`
                        name_a = Foo
                        name_b = Bar
                the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
                  judgment `sub { a: my(class(Foo)), b: my(class(Bar)) }` failed at the following rule(s):
                    the rule "both my" failed at step #0 (src/file.rs:LL:CC) because
                      judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
                        the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
                          condition evaluted to false: `name_a == name_b`
                            name_a = Foo
                            name_b = Bar
                    the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
                      judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
                        the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
                          condition evaluted to false: `name_a == name_b`
                            name_a = Foo
                            name_b = Bar
            the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
              judgment `sub { a: my(class(Foo)), b: my(class(Bar)) }` failed at the following rule(s):
                the rule "both my" failed at step #0 (src/file.rs:LL:CC) because
                  judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
                    the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
                      condition evaluted to false: `name_a == name_b`
                        name_a = Foo
                        name_b = Bar
                the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
                  judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
                    the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
                      condition evaluted to false: `name_a == name_b`
                        name_a = Foo
                        name_b = Bar
                the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
                  judgment `sub { a: class(Foo), b: my(class(Bar)) }` failed at the following rule(s):
                    the rule "equivalent" failed at step #3 (src/file.rs:LL:CC) because
                      judgment `sub { a: class(Foo), b: class(Bar) }` failed at the following rule(s):
                        the rule "same class" failed at step #0 (src/file.rs:LL:CC) because
                          condition evaluted to false: `name_a == name_b`
                            name_a = Foo
                            name_b = Bar"#]]
    );
}
