use a_mir_formality::test_where_clause;

#[test]
fn associated_item_requirements_are_not_trait_guarantees() {
    let result = test_where_clause(
        "[
            crate test {
                trait Item { type Out: []; }
                trait Family<T> where T: Item {
                    type View<'a>: [] where 'a: 'static, <T as Item>::Out => u32;
                }
            }
        ]",
        "forall<C, T> { Item(T), Family(C, T) } => { for<'a> <T as Item>::Out = u32 }",
    );
    assert!(!result.iter().any(|(c, _)| c.known_true));
}

const GAT_FAMILY: &str = "[
    crate test {
        trait Family {
            type View<'a> : [];
        }

        trait FnLike<Args> {
            type Output : [];
        }

        struct IdentityFn {}

        impl<X> FnLike<(X,)> for IdentityFn {
            type Output = X;
        }

        struct Borrowed {}
        struct Erased {}

        impl Family for Borrowed {
            type View<'a> = &'a u32;
        }

        impl Family for Erased {
            type View<'a> = ();
        }
    }
]";

const DECLARED_EQUALITY: &str = "[
    crate test {
        trait Item {
            type Out : [];
        }

        trait Left<X> where X: Item {
            type Assoc : [Item::Out => <X as Item>::Out];
        }
    }
]";

const GAT_DECLARED_EQUALITY: &str = "[
    crate test {
        trait Item {
            type Out : [];
        }

        trait Family<X> where X: Item {
            type View<'a> : [Item::Out => <X as Item>::Out];
        }
    }
]";

#[test]
fn same_complete_input_and_output_types_support_a_higher_ranked_bound() {
    assert!(test_where_clause(
        GAT_FAMILY,
        "forall<T> { Family(T) } => { for<'a> FnLike(IdentityFn, (<T as Family>::View<'a>,)), for<'a> <IdentityFn as FnLike<(<T as Family>::View<'a>,)> >::Output = <T as Family>::View<'a> }",
    )
    .iter().any(|(c, _)| c.unconditionally_true()));

    assert!(!test_where_clause(
        GAT_FAMILY,
        "forall<T, U> { Family(T), Family(U) } => { for<'a> FnLike(IdentityFn, (<T as Family>::View<'a>,)), for<'a> <IdentityFn as FnLike<(<T as Family>::View<'a>,)> >::Output = <U as Family>::View<'a> }",
    )
    .iter().any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn independent_equality_connects_distinct_families() {
    assert!(test_where_clause(
        GAT_FAMILY,
        "forall<T, U> {
            Family(T), Family(U),
            for<'a> <T as Family>::View<'a> = <U as Family>::View<'a>
        } => {
            for<'b> <IdentityFn as FnLike<(<T as Family>::View<'b>,)>>::Output =
            <U as Family>::View<'b>
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn gat_projection_can_be_normalized_for_a_bound_lifetime() {
    assert!(test_where_clause(
        GAT_FAMILY,
        "forall<'a> {} => { Family(Borrowed), <Borrowed as Family>::View<'a> = &'a u32 }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn an_erased_gat_does_not_recover_its_lifetime_argument() {
    assert!(test_where_clause(
        GAT_FAMILY,
        "forall<'a, 'b> {} => { Family(Erased), <Erased as Family>::View<'a> = <Erased as Family>::View<'b> }",
    )
    .iter().any(|(c, _)| c.unconditionally_true()));

    assert!(!test_where_clause(
        GAT_FAMILY,
        "forall<'a, 'b> { Family(Erased), <Erased as Family>::View<'a> = <Erased as Family>::View<'b> } => { 'a = 'b }",
    )
    .iter().any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn quantified_erased_equality_normalizes_forward_without_leaking_a_reverse_witness() {
    use formality_rust::grammar::{Crates, Predicate, Wc};
    use formality_rust::prove::test_util::TestAssertionPart;
    use formality_rust::prove::{prove_normalize, Constrained, Env};
    use formality_rust::rust::term;

    let crates: Crates = term(GAT_FAMILY);
    let TestAssertionPart::ForAll(binder) = term(
        "forall<T, 'known> {
            Family(T), for<'a> <T as Family>::View<'a> = ()
        } => { <T as Family>::View<'known> = () }",
    ) else {
        panic!();
    };
    let (env, assertion) = Env::default().instantiate_universally(&binder);
    let TestAssertionPart::Prove(assumptions, goals) = &*assertion else {
        panic!();
    };
    let Wc::Predicate(Predicate::Equals(projection, erased)) = goals.iter().next().unwrap() else {
        panic!();
    };

    let forward = prove_normalize(crates.to_prove_decls(), &env, assumptions, &projection);
    assert!(
        forward
            .iter()
            .any(|(Constrained(output, c), _)| { output == erased && c.unconditionally_true() }),
        "{forward:?}"
    );

    let reverse = prove_normalize(crates.to_prove_decls(), env, assumptions, &erased);
    for (Constrained(output, c), _) in forward.iter().chain(reverse.iter()) {
        assert!(c.env().encloses(&output), "{output:?}: {c:?}");
    }
    assert!(
        !reverse.iter().any(|(Constrained(_, c), _)| c.known_true),
        "{reverse:?}"
    );
}

#[test]
fn declaration_equality_follows_an_associated_type_substitution() {
    assert!(test_where_clause(
        DECLARED_EQUALITY,
        "forall<C, T, U> { Item(T), Item(U), Left(C, U), <C as Left<U>>::Assoc = T } => { <T as Item>::Out = <U as Item>::Out }",
    )
    .iter().any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn declaration_equalities_compose_only_when_both_edges_are_present() {
    let premises = "Item(T), Item(U), Item(V), Left(C, U), Left(D, V)";
    let first_edge = "<C as Left<U>>::Assoc = T";
    let second_edge = "<D as Left<V>>::Assoc = U";
    for (edges, expected) in [
        (first_edge.to_owned(), false),
        (second_edge.to_owned(), false),
        (format!("{first_edge}, {second_edge}"), true),
    ] {
        let assertion = format!(
            "forall<C, D, T, U, V> {{ {premises}, {edges} }} => {{
                <T as Item>::Out = <V as Item>::Out
            }}"
        );
        let result = test_where_clause(DECLARED_EQUALITY, &assertion);
        if expected {
            assert!(
                result.iter().any(|(c, _)| c.unconditionally_true()),
                "{assertion}"
            );
        } else {
            assert!(!result.iter().any(|(c, _)| c.known_true), "{assertion}");
        }
    }
}

#[test]
fn declaration_can_supply_a_direct_projection_equality() {
    assert!(test_where_clause(
        DECLARED_EQUALITY,
        "forall<C, U> { Item(U), Left(C, U) } => { <<C as Left<U>>::Assoc as Item>::Out = <U as Item>::Out }",
    )
    .iter().any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn declared_gat_equality_preserves_its_lifetime_binder() {
    assert!(test_where_clause(
        GAT_DECLARED_EQUALITY,
        "forall<C, X> { Item(X), Family(C, X) } => { for<'a> <<C as Family<X>>::View<'a> as Item>::Out = <X as Item>::Out }",
    )
    .iter().any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn associated_guarantee_requires_its_item_premises() {
    let program = "[
        crate test {
            trait Item { type Out: []; }
            trait Family {
                type View<'a>: [Item::Out => &'a u32] where 'a: 'static;
            }
        }
    ]";
    assert!(test_where_clause(
        program,
        "forall<C> { Family(C) } => { <<C as Family>::View<'static> as Item>::Out = &'static u32 }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        program,
        "forall<C> { Family(C) } => { for<'a> <<C as Family>::View<'a> as Item>::Out = &'a u32 }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn quantified_source_and_goal_keep_their_lifetimes_distinct() {
    let program = "[
        crate test {
            trait Item { type Out: []; }
            trait Family<'a> { type View: [Item::Out => &'a u32]; }
        }
    ]";
    assert!(test_where_clause(
        program,
        "forall<C> { for<'a> Family(C, 'a) } => { for<'b> <<C as Family<'b>>::View as Item>::Out = &'b u32 }",
    ).iter().any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        program,
        "forall<C, 'a, 'b> { Family(C, 'a) } => { <<C as Family<'a>>::View as Item>::Out = &'b u32 }",
    ).iter().any(|(c, _)| c.known_true));
}

#[test]
fn declarations_require_an_established_trait_assumption() {
    assert!(!test_where_clause(
        DECLARED_EQUALITY,
        "forall<C, U> { Item(U) } => { <<C as Left<U>>::Assoc as Item>::Out = <U as Item>::Out }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
    assert!(!test_where_clause(
        DECLARED_EQUALITY,
        "forall<C, U> { Item(U), Left(C, U) } => { <U as Item>::Out = u32 }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn impl_cannot_use_its_own_associated_guarantee_as_a_premise() {
    let program = "[
        crate test {
            trait Item { type Out: []; }
            impl Item for bool { type Out = bool; }
            trait Claim { type Assoc: [Item::Out => u32]; }
            impl Claim for () where <bool as Item>::Out => u32 {
                type Assoc = bool;
            }
        }
    ]";
    assert!(!test_where_clause(program, "{} => { Claim(()) }")
        .iter()
        .any(|(c, _)| c.known_true));
}

#[test]
fn an_impl_cannot_establish_an_equality_by_assuming_its_own_trait() {
    let program = "[
        crate test {
            trait Item { type Out: []; }
            impl Item for bool { type Out = bool; }
            trait Claim { type Assoc: [Item::Out => u32]; }
            impl Claim for () where <<() as Claim>::Assoc as Item>::Out => u32 {
                type Assoc = bool;
            }
        }
    ]";
    assert!(!test_where_clause(program, "{} => { Claim(()) }")
        .iter()
        .any(|(c, _)| c.known_true));
}

#[test]
fn an_impl_cannot_establish_an_associated_bound_by_assuming_its_own_trait() {
    let program = "[
        crate test {
            trait Required {}
            trait Claim { type Assoc: [Required]; }
            impl Claim for () where <() as Claim>::Assoc: Required {
                type Assoc = bool;
            }
        }
    ]";
    assert!(!test_where_clause(program, "{} => { Claim(()) }")
        .iter()
        .any(|(c, _)| c.known_true));
}

#[test]
fn impl_must_establish_the_associated_guarantee() {
    let result = a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Item { type Out: []; }
            impl Item for bool { type Out = bool; }
            trait Claim { type Assoc: [Item::Out => u32]; }
            impl Claim for () { type Assoc = bool; }
        }
    ]",
    );
    assert!(result.is_err());
}

#[test]
fn associated_equality_bounds_translate_to_rust() {
    a_mir_formality::FormalityTest::new(a_mir_formality::crates![crate test {
        trait Producer<'a> { type Value<T>: []; }
        impl<'a> Producer<'a> for () { type Value<T> = T; }
        trait Carrier { type Assoc: [Producer<'static>::Value<u32> => u32]; }
        impl Carrier for bool { type Assoc = (); }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn explicit_projection_equalities_can_be_composed() {
    let program = "[
        crate test {
            trait Item { type Out: []; }
            trait Family { type Assoc: [Item]; }
        }
    ]";
    assert!(test_where_clause(
        program,
        "forall<C, T, U> {
            Family(C), Item(T), Item(U),
            <C as Family>::Assoc = T,
            <<C as Family>::Assoc as Item>::Out = <U as Item>::Out
        } => { <T as Item>::Out = <U as Item>::Out }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        program,
        "forall<C, T, U> {
            Family(C), Item(T), Item(U),
            <C as Family>::Assoc = T
        } => { <T as Item>::Out = <U as Item>::Out }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn supertrait_supplies_its_associated_equality() {
    assert!(test_where_clause(
        "[
            crate test {
                trait Item { type Out: []; }
                trait Parent { type Assoc: [Item::Out => u32]; }
                trait Child where Self: Parent {}
            }
        ]",
        "forall<C> { Child(C) } => { <<C as Parent>::Assoc as Item>::Out = u32 }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn nested_associated_trait_supplies_its_equality() {
    assert!(test_where_clause(
        "[
            crate test {
                trait Item { type Out: []; }
                trait Middle { type Assoc: [Item::Out => u32]; }
                trait Outer { type Assoc: [Middle]; }
            }
        ]",
        "forall<C> { Outer(C) } => {
            Middle(<C as Outer>::Assoc),
            <<<C as Outer>::Assoc as Middle>::Assoc as Item>::Out = u32
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn nested_declaration_keeps_the_outer_gat_requirement() {
    let program = "[
        crate test {
            trait Item { type Out: []; }
            trait Middle { type Assoc: [Item::Out => u32]; }
            trait Outer { type View<'a>: [Middle] where 'a: 'static; }
        }
    ]";
    assert!(test_where_clause(
        program,
        "forall<C> { Outer(C) } => {
            <<<C as Outer>::View<'static> as Middle>::Assoc as Item>::Out = u32
        }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        program,
        "forall<C> { Outer(C) } => {
            for<'a> <<<C as Outer>::View<'a> as Middle>::Assoc as Item>::Out = u32
        }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn expanding_declarations_stop_without_proving_an_unrelated_equality() {
    use formality_rust::grammar::Crates;
    use formality_rust::prove::test_util::test_prove;
    use formality_rust::rust::term;

    let program = "[
        crate test {
            trait Recursive { type Next: [Recursive]; }
        }
    ]";
    assert!(a_mir_formality::test_program_ok(program).is_ok());
    let crates: Crates = term(program);
    let mut decls = crates.to_prove_decls();
    decls.max_size = 16;
    let result = test_prove(
        decls,
        term("forall<T> { Recursive(T) } => { <T as Recursive>::Next = u32 }"),
    );
    assert!(result.iter().any(|(c, _)| !c.known_true));
    assert!(!result.iter().any(|(c, _)| c.known_true));
}

#[test]
fn coincident_trait_arguments_do_not_turn_requirements_into_supertraits() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Required {}
            trait Wrapper<T> where T: Required {}
            trait User<T> where T: Wrapper<T> {}
        }
    ]"
    )
    .is_err());
}

#[test]
fn trait_parameter_equalities_remain_requirements() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Item { type Out: []; }
            trait Wrapper<T> where T: Item, <T as Item>::Out => u32 {}
            trait User<T> where T: Item, T: Wrapper<T> {}
        }
    ]"
    )
    .is_err());
}

#[test]
fn supertrait_associated_equality_is_a_guarantee() {
    assert!(test_where_clause(
        "[
            crate test {
                trait Item { type Out: []; }
                trait Wrapper where Self: Item, <Self as Item>::Out => u32 {}
            }
        ]",
        "forall<T> { Wrapper(T) } => { <T as Item>::Out = u32 }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn associated_equality_must_name_a_declared_item() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Item { type Out: []; }
            trait Family { type Assoc: [Item::Missing => u32]; }
        }
    ]"
    )
    .is_err());
}

#[test]
fn associated_equality_must_name_a_declared_trait() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Family { type Assoc: [Missing::Out => u32]; }
        }
    ]"
    )
    .is_err());
}

#[test]
fn associated_equality_arguments_match_their_declarations() {
    for bound in [
        "Item::Out<'static> => u32",
        "Item<'static>::Out<'static> => u32",
        "Item<u32, bool>::Out<'static> => u32",
        "Item<u32>::Out => u32",
        "Item<u32>::Out<u32> => u32",
        "Item<u32>::Out<'static, 'static> => u32",
    ] {
        let program = format!(
            "[
            crate test {{
                trait Item<T> {{ type Out<'a>: []; }}
                trait Family {{ type Assoc: [{bound}]; }}
            }}
        ]"
        );
        assert!(
            a_mir_formality::test_program_ok(&program).is_err(),
            "{bound}"
        );
    }
}

#[test]
fn associated_equality_requires_trait_prerequisites() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Required {}
            trait Item<T> where T: Required { type Out: []; }
            trait Family { type Assoc: [Item<u32>::Out => u32]; }
        }
    ]"
    )
    .is_err());
}

#[test]
fn associated_equality_requires_item_prerequisites() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Item { type Out<'a>: [] where 'a: 'static; }
            trait Family { type View<'a>: [Item::Out<'a> => u32]; }
        }
    ]"
    )
    .is_err());
}

#[test]
fn associated_equality_requires_a_well_formed_output() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Item { type Out: []; }
            trait Family<T> { type Assoc: [Item::Out => &'static T]; }
        }
    ]"
    )
    .is_err());
}

#[test]
fn associated_equality_accepts_guarded_well_formed_bounds() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Required {}
            trait Item<T> where T: Required { type Out<'a>: [] where 'a: 'static; }
            trait Family<T> where T: Required {
                type View<'a>: [Item<T>::Out<'a> => &'a T] where 'a: 'static, T: 'a;
            }
        }
    ]"
    )
    .is_ok());
}

#[test]
fn associated_equality_can_use_a_sibling_associated_bound() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Required {}
            trait Item<T> where T: Required { type Out: []; }
            trait Family {
                type Input: [Required];
                type Output: [Item<<Self as Family>::Input>::Out => u32];
            }
        }
    ]"
    )
    .is_ok());
}

#[test]
fn higher_ranked_associated_equalities_preserve_their_bound_lifetime() {
    let program = "[
        crate test {
            trait Item { type Out<'a>: []; }
            trait Family { type Assoc: [for<'a> Item::Out<'a> => &'a u32]; }
        }
    ]";
    assert!(test_where_clause(
        program,
        "forall<C> { Family(C) } => { for<'b> <<C as Family>::Assoc as Item>::Out<'b> = &'b u32 }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
    assert!(!test_where_clause(
        program,
        "forall<C, 'a, 'b> { Family(C) } => { <<C as Family>::Assoc as Item>::Out<'a> = &'b u32 }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn higher_ranked_associated_equalities_require_all_item_instances_to_be_valid() {
    assert!(a_mir_formality::test_program_ok(
        "[
        crate test {
            trait Item { type Out<'a>: [] where 'a: 'static; }
            trait Family { type Assoc: [for<'a> Item::Out<'a> => u32]; }
        }
    ]"
    )
    .is_err());
}
