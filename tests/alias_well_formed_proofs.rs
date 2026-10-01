use a_mir_formality::{test_program_ok, test_where_clause};

#[test]
fn equality_outputs_require_declared_aliases() {
    for ty in ["<X as Missing>::Out", "<X as Item>::Missing"] {
        let program = format!(
            "[crate test {{
                trait Item {{ type Out: []; }}
                trait Family<X> where X: Item {{ type Assoc: [Item::Out => {ty}]; }}
            }}]"
        );
        assert!(test_program_ok(&program).is_err(), "{ty}");
    }
}

#[test]
fn equality_outputs_require_matching_alias_arguments() {
    for ty in [
        "<X as Generic>::Out<'static>",
        "<X as Generic<'static>>::Out<'static>",
        "<X as Generic<u32, bool>>::Out<'static>",
        "<X as Generic<u32>>::Out",
        "<X as Generic<u32>>::Out<u32>",
        "<X as Generic<u32>>::Out<'static, 'static>",
    ] {
        let program = format!(
            "[crate test {{
                trait Item {{ type Out: []; }}
                trait Generic<T> {{ type Out<'a>: []; }}
                trait Family<X> where X: Generic<u32> {{ type Assoc: [Item::Out => {ty}]; }}
            }}]"
        );
        assert!(test_program_ok(&program).is_err(), "{ty}");
    }
}

#[test]
fn equality_outputs_require_the_alias_trait() {
    assert!(test_program_ok(
        "[crate test {
            trait Item { type Out: []; }
            trait Family<X> { type Assoc: [Item::Out => <X as Item>::Out]; }
        }]"
    )
    .is_err());
}

#[test]
fn equality_outputs_require_the_alias_item_premises() {
    assert!(test_program_ok(
        "[crate test {
            trait Item { type Out: []; }
            trait Guarded { type Out<'a>: [] where 'a: 'static; }
            trait Family<X> where X: Guarded {
                type Assoc<'a>: [Item::Out => <X as Guarded>::Out<'a>];
            }
        }]"
    )
    .is_err());
}

#[test]
fn equality_outputs_accept_guarded_self_and_sibling_aliases() {
    let _ = test_program_ok(
        "[crate test {
            trait Item { type Out: []; }
            trait Family {
                type Input<'a>: [] where 'a: 'static;
                type Output<'a>: [Item::Out => <Self as Family>::Input<'a>] where 'a: 'static;
            }
        }]",
    )
    .unwrap();
}

#[test]
fn alias_well_formedness_requires_its_trait_and_item_premises() {
    let program = "[crate test {
        trait Item { type Out<'a>: [] where 'a: 'static; }
    }]";
    for (assumptions, succeeds) in [
        ("", false),
        ("Item(T)", false),
        ("'a: 'static", false),
        ("Item(T), 'a: 'static", true),
    ] {
        let goal = format!("forall<T, 'a> {{ {assumptions} }} => {{ @wf(<T as Item>::Out<'a>) }}");
        let result = test_where_clause(program, &goal);
        assert_eq!(result.iter().any(|(c, _)| c.known_true), succeeds, "{goal}");
    }
}

#[test]
fn alias_well_formedness_requires_trait_parameter_premises() {
    let program = "[crate test {
        trait Required {}
        trait Item<X> where X: Required { type Out: []; }
    }]";
    for (assumptions, succeeds) in [("Item(T, X)", false), ("Item(T, X), Required(X)", true)] {
        let goal = format!("forall<T, X> {{ {assumptions} }} => {{ @wf(<T as Item<X>>::Out) }}");
        assert_eq!(
            test_where_clause(program, &goal)
                .iter()
                .any(|(c, _)| c.known_true),
            succeeds,
            "{goal}"
        );
    }
}

#[test]
fn trait_methods_can_name_their_own_associated_types() {
    let _ = test_program_ok(
        "[crate test {
            trait Item {
                type Out: [];
                fn produce() -> <Self as Item>::Out;
            }
        }]",
    )
    .unwrap();
}

#[test]
fn associated_item_premises_can_name_sibling_associated_types() {
    let _ = test_program_ok(
        "[crate test {
            trait Required {}
            trait Item {
                type Input: [];
                type Out: [] where <Self as Item>::Input: Required;
            }
        }]",
    )
    .unwrap();
}

#[test]
fn trait_premises_can_name_their_own_associated_types() {
    let _ = test_program_ok(
        "[crate test {
            trait Required {}
            trait Item where <Self as Item>::Input: Required {
                type Input: [];
            }
        }]",
    )
    .unwrap();
}
