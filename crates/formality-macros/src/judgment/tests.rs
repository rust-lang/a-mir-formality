use super::parse::parse;

use quote::quote;

#[test]
fn parse_axiom() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)

            (
                ------------------------------------------------------------ ("always true")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
fn parse_assertion() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            assert(f == f)

            (
                ------------------------------------------------------------ ("always true")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
fn parse_trivials() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                ------------------------------------------------------------ ("always true")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
fn parse_if() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (if f == "this is a function")
                ------------------------------------------------------------ ("if")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
fn parse_fail() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (fail "error")
                ------------------------------------------------------------ ("always fails")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
fn parse_fail_with_expr() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (fail "error {}", f)
                ------------------------------------------------------------ ("always fails")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
fn parse_implication() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (check_fn(program, Env::default(), Predicates::t(), f, crate_id) => ())
                ------------------------------------------------------------ ("check fn")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
fn parse_in() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (wc in a.where_clauses.iter().chain(&b.where_clauses).flat_map(|wc| wc.invert()))
                --- ("foo")
                (overlap_check_impl(program, impl_a, impl_b) => ())
            )
        }
    });
}

#[test]
fn parse_let() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (let (env, a) = Env::default().instantiate_universally(&impl_a.binder))
                --- ("foo")
                (overlap_check_impl(program, impl_a, impl_b) => ())
            )
        }
    });
}

#[test]
fn parse_for_all() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)

            (
                (for_all(field in fields)
                 (let Field { ty, .. } = field)
                 (prove_goal(program, env, where_clauses, AtomicPredicate::well_formed(ty)) => ()))
                --- ("foo")
                (overlap_check_impl(program, impl_a, impl_b) => ())
            )
        }
    });
}

#[test]
#[ignore = "not implemented yet"]
fn parse_for_all_with_state() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)

            (
                (for_all(field in fields with(state))
                 (let Field { ty, .. } = field)
                 (prove_goal(program, env, where_clauses, AtomicPredicate::well_formed(ty)) => ()))
                --- ("foo")
                (overlap_check_impl(program, impl_a, impl_b) => ())
            )
        }
    });
}

#[test]
#[ignore = "not implemented yet"]
fn parse_if_let() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (if let PlaceExpr::Var(id) = place)
                ------------------------------------------------------------ ("if let")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
#[ignore = "not implemented yet"]
fn parse_let_for_all() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (let expected_names: Set<FieldName> = for_all field in fields => field.name)
                ------------------------------------------------------------ ("let for all")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}

#[test]
#[ignore = "not implemented yet"]
fn parse_scope() {
    parse(quote! {
        pub(crate) fn foo(
            program: Program,
            f: Fn,
            crate_id: CrateId,
        ) => () {
            debug(f, crate_id, program)
            trivial(f => ())

            (
                (scope(enter_universally(env, &binder.explicit_binder) => (env, TraitBoundData { where_clauses, trait_items })) with())
                ------------------------------------------------------------ ("scope")
                (foo(program, f, crate_id) => ())
            )
        }
    });
}
