use a_mir_formality::test_where_clause;

const DECLARATIONS: &str = "[
    crate test {
        trait Required {}
        trait Provider { type Assoc: [Required]; }
        trait Claim where Self: Provider {}
    }
]";

#[test]
fn provisional_supertrait_does_not_supply_an_associated_guarantee() {
    assert!(!test_where_clause(
        DECLARATIONS,
        "{ @coinductive(Claim(())) } => { Required(<() as Provider>::Assoc) }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn established_supertrait_supplies_an_associated_guarantee() {
    assert!(test_where_clause(
        DECLARATIONS,
        "forall<T> { Claim(T) } => { Required(<T as Provider>::Assoc) }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn provisional_trait_cannot_discharge_an_associated_item_requirement() {
    let program = "[
        crate test {
            trait Required {}
            trait Claim {}
            trait Outer { type Assoc: [Required] where Self: Claim; }
        }
    ]";
    assert!(!test_where_clause(
        program,
        "forall<T> { Outer(T), @coinductive(Claim(T)) } => { Required(<T as Outer>::Assoc) }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
    assert!(test_where_clause(
        program,
        "forall<T> { Outer(T), Claim(T) } => { Required(<T as Outer>::Assoc) }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn alias_normalization_does_not_close_an_impl_cycle() {
    assert!(!test_where_clause(
        "[
            crate test {
                trait Item { type Out: []; }
                trait Claim {}
                impl Item for bool where (): Claim { type Out = u32; }
                impl Claim for () where <bool as Item>::Out => u32 {}
            }
        ]",
        "{} => { Claim(()) }",
    )
    .iter()
    .any(|(c, _)| c.known_true));
}

#[test]
fn independently_established_impl_allows_alias_normalization() {
    assert!(test_where_clause(
        "[
            crate test {
                trait Item { type Out: []; }
                trait Claim {}
                impl Item for bool where (): Claim { type Out = u32; }
                impl Claim for () where <bool as Item>::Out => u32 {}
            }
        ]",
        "{ Claim(()) } => { <bool as Item>::Out = u32 }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}

#[test]
fn trait_only_cycles_remain_coinductive() {
    assert!(test_where_clause(
        "[
            crate test {
                trait Left {}
                trait Right {}
                impl Left for () where (): Right {}
                impl Right for () where (): Left {}
            }
        ]",
        "{} => { Left(()), Right(()) }",
    )
    .iter()
    .any(|(c, _)| c.unconditionally_true()));
}
