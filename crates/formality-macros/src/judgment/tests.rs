use std::cell::LazyCell;

use crate::judgment::ast::Premises;
use crate::judgment::{ast, codegen};

use crate::judgment::parse::parse;

use expect_test::expect;
use quote::quote;
use syn::parse::Parser;

use proc_macro2::TokenStream;

const DUMMY: LazyCell<TokenStream> = LazyCell::new(|| quote!({ dummy }));

fn render_block(tokens: TokenStream) -> String {
    let file: syn::File = syn::parse2(quote! {
        fn __dummy() {
            #tokens
        }
    })
    .expect("generated code should be valid block statements");

    prettyplease::unparse(&file)
}

#[test]
fn parse_axiom() {
    let ast = parse(quote! {
        pub fn foo(
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
    expect![[r#"
        Judgment {
            attributes: [],
            signature: Signature {
                visibility: Visibility::Public(
                    Pub,
                ),
                name: Ident(
                    foo,
                ),
                arguments: [
                    Argument {
                        name: Ident(
                            program,
                        ),
                        ty: Type::Path {
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            Program,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                    },
                    Argument {
                        name: Ident(
                            f,
                        ),
                        ty: Type::Path {
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            Fn,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                    },
                    Argument {
                        name: Ident(
                            crate_id,
                        ),
                        ty: Type::Path {
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            CrateId,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                    },
                ],
                return_: Type::Tuple {
                    paren_token: Paren,
                    elems: [],
                },
            },
            body: JudgmentBody {
                debug: Debug {
                    idents: [
                        Ident(
                            f,
                        ),
                        Ident(
                            crate_id,
                        ),
                        Ident(
                            program,
                        ),
                    ],
                },
                asserts: Asserts {
                    assertions: [],
                },
                trivials: Trivials {
                    trivials: [],
                },
                rules: Rules {
                    rules: [
                        Rule {
                            premises: Premises {
                                premises: [],
                            },
                            name: "always true",
                            conclusion: Conclusion {
                                name: Ident(
                                    foo,
                                ),
                                patterns: [
                                    Pat::Ident {
                                        attrs: [],
                                        by_ref: None,
                                        mutability: None,
                                        ident: Ident(
                                            program,
                                        ),
                                        subpat: None,
                                    },
                                    Pat::Ident {
                                        attrs: [],
                                        by_ref: None,
                                        mutability: None,
                                        ident: Ident(
                                            f,
                                        ),
                                        subpat: None,
                                    },
                                    Pat::Ident {
                                        attrs: [],
                                        by_ref: None,
                                        mutability: None,
                                        ident: Ident(
                                            crate_id,
                                        ),
                                        subpat: None,
                                    },
                                ],
                                result: Expr::Tuple {
                                    attrs: [],
                                    paren_token: Paren,
                                    elems: [],
                                },
                            },
                        },
                    ],
                },
            },
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_assertion() {
    let ast: ast::Asserts = syn::parse2(quote! {
        assert(f == f)
    })
    .unwrap();

    expect![[r#"
        Asserts {
            assertions: [
                Assert {
                    assertion: Expr::Binary {
                        attrs: [],
                        left: Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            f,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                        op: BinOp::Eq(
                            EqEq,
                        ),
                        right: Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            f,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                    },
                },
            ],
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_trivials() {
    let ast: ast::Trivials = syn::parse2(quote! {
            trivial(f => ())
    })
    .unwrap();
    expect![[r#"
        Trivials {
            trivials: [
                Trivial {
                    hypothesis: Expr::Path {
                        attrs: [],
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: [
                                PathSegment {
                                    ident: Ident(
                                        f,
                                    ),
                                    arguments: PathArguments::None,
                                },
                            ],
                        },
                    },
                    conclusion: Expr::Tuple {
                        attrs: [],
                        paren_token: Paren,
                        elems: [],
                    },
                },
            ],
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_if() {
    let ast: ast::Premise = syn::parse2(quote! {
        (if f == "this is a function")
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: If(
                If {
                    expression: Expr::Binary {
                        attrs: [],
                        left: Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            f,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                        op: BinOp::Eq(
                            EqEq,
                        ),
                        right: Expr::Lit {
                            attrs: [],
                            lit: Lit::Str {
                                token: "this is a function",
                            },
                        },
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_fail() {
    let ast: ast::Premise = syn::parse2(quote! {
        (fail "error")
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: Fail(
                Fail {
                    message: LitStr {
                        token: "error",
                    },
                    expressions: [],
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_fail_with_expr() {
    let ast: ast::Premise = syn::parse2(quote! {
        (fail "error {}", f)
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: Fail(
                Fail {
                    message: LitStr {
                        token: "error {}",
                    },
                    expressions: [
                        Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            f,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                    ],
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_implication() {
    let ast: ast::Premise = syn::parse2(quote! {
        (check_fn(program, Env::default(), Predicates::t(), f, crate_id) => ())
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: Implication(
                Implication {
                    hypothesis: Expr::Call {
                        attrs: [],
                        func: Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            check_fn,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                        paren_token: Paren,
                        args: [
                            Expr::Path {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                program,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                            },
                            Comma,
                            Expr::Call {
                                attrs: [],
                                func: Expr::Path {
                                    attrs: [],
                                    qself: None,
                                    path: Path {
                                        leading_colon: None,
                                        segments: [
                                            PathSegment {
                                                ident: Ident(
                                                    Env,
                                                ),
                                                arguments: PathArguments::None,
                                            },
                                            PathSep,
                                            PathSegment {
                                                ident: Ident(
                                                    default,
                                                ),
                                                arguments: PathArguments::None,
                                            },
                                        ],
                                    },
                                },
                                paren_token: Paren,
                                args: [],
                            },
                            Comma,
                            Expr::Call {
                                attrs: [],
                                func: Expr::Path {
                                    attrs: [],
                                    qself: None,
                                    path: Path {
                                        leading_colon: None,
                                        segments: [
                                            PathSegment {
                                                ident: Ident(
                                                    Predicates,
                                                ),
                                                arguments: PathArguments::None,
                                            },
                                            PathSep,
                                            PathSegment {
                                                ident: Ident(
                                                    t,
                                                ),
                                                arguments: PathArguments::None,
                                            },
                                        ],
                                    },
                                },
                                paren_token: Paren,
                                args: [],
                            },
                            Comma,
                            Expr::Path {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                f,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                            },
                            Comma,
                            Expr::Path {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                crate_id,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                            },
                        ],
                    },
                    conclusion: Pat::Tuple {
                        attrs: [],
                        paren_token: Paren,
                        elems: [],
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_in() {
    let ast: ast::Premise = syn::parse2(quote! {
        (wc in a.where_clauses.iter().chain(&b.where_clauses).flat_map(|wc| wc.invert()))
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: In(
                In {
                    pattern: Pat::Ident {
                        attrs: [],
                        by_ref: None,
                        mutability: None,
                        ident: Ident(
                            wc,
                        ),
                        subpat: None,
                    },
                    expression: Expr::MethodCall {
                        attrs: [],
                        receiver: Expr::MethodCall {
                            attrs: [],
                            receiver: Expr::MethodCall {
                                attrs: [],
                                receiver: Expr::Field {
                                    attrs: [],
                                    base: Expr::Path {
                                        attrs: [],
                                        qself: None,
                                        path: Path {
                                            leading_colon: None,
                                            segments: [
                                                PathSegment {
                                                    ident: Ident(
                                                        a,
                                                    ),
                                                    arguments: PathArguments::None,
                                                },
                                            ],
                                        },
                                    },
                                    dot_token: Dot,
                                    member: Member::Named(
                                        Ident(
                                            where_clauses,
                                        ),
                                    ),
                                },
                                dot_token: Dot,
                                method: Ident(
                                    iter,
                                ),
                                turbofish: None,
                                paren_token: Paren,
                                args: [],
                            },
                            dot_token: Dot,
                            method: Ident(
                                chain,
                            ),
                            turbofish: None,
                            paren_token: Paren,
                            args: [
                                Expr::Reference {
                                    attrs: [],
                                    and_token: And,
                                    mutability: None,
                                    expr: Expr::Field {
                                        attrs: [],
                                        base: Expr::Path {
                                            attrs: [],
                                            qself: None,
                                            path: Path {
                                                leading_colon: None,
                                                segments: [
                                                    PathSegment {
                                                        ident: Ident(
                                                            b,
                                                        ),
                                                        arguments: PathArguments::None,
                                                    },
                                                ],
                                            },
                                        },
                                        dot_token: Dot,
                                        member: Member::Named(
                                            Ident(
                                                where_clauses,
                                            ),
                                        ),
                                    },
                                },
                            ],
                        },
                        dot_token: Dot,
                        method: Ident(
                            flat_map,
                        ),
                        turbofish: None,
                        paren_token: Paren,
                        args: [
                            Expr::Closure {
                                attrs: [],
                                lifetimes: None,
                                constness: None,
                                movability: None,
                                asyncness: None,
                                capture: None,
                                or1_token: Or,
                                inputs: [
                                    Pat::Ident {
                                        attrs: [],
                                        by_ref: None,
                                        mutability: None,
                                        ident: Ident(
                                            wc,
                                        ),
                                        subpat: None,
                                    },
                                ],
                                or2_token: Or,
                                output: ReturnType::Default,
                                body: Expr::MethodCall {
                                    attrs: [],
                                    receiver: Expr::Path {
                                        attrs: [],
                                        qself: None,
                                        path: Path {
                                            leading_colon: None,
                                            segments: [
                                                PathSegment {
                                                    ident: Ident(
                                                        wc,
                                                    ),
                                                    arguments: PathArguments::None,
                                                },
                                            ],
                                        },
                                    },
                                    dot_token: Dot,
                                    method: Ident(
                                        invert,
                                    ),
                                    turbofish: None,
                                    paren_token: Paren,
                                    args: [],
                                },
                            },
                        ],
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_let() {
    let ast: ast::Premise = syn::parse2(quote! {
        (let (env, a) = Env::default().instantiate_universally(&impl_a.binder))
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: Let(
                Let {
                    binding: Pattern {
                        pattern: Pat::Tuple {
                            attrs: [],
                            paren_token: Paren,
                            elems: [
                                Pat::Ident {
                                    attrs: [],
                                    by_ref: None,
                                    mutability: None,
                                    ident: Ident(
                                        env,
                                    ),
                                    subpat: None,
                                },
                                Comma,
                                Pat::Ident {
                                    attrs: [],
                                    by_ref: None,
                                    mutability: None,
                                    ident: Ident(
                                        a,
                                    ),
                                    subpat: None,
                                },
                            ],
                        },
                    },
                    expression: Expr::MethodCall {
                        attrs: [],
                        receiver: Expr::Call {
                            attrs: [],
                            func: Expr::Path {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                Env,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                        PathSep,
                                        PathSegment {
                                            ident: Ident(
                                                default,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                            },
                            paren_token: Paren,
                            args: [],
                        },
                        dot_token: Dot,
                        method: Ident(
                            instantiate_universally,
                        ),
                        turbofish: None,
                        paren_token: Paren,
                        args: [
                            Expr::Reference {
                                attrs: [],
                                and_token: And,
                                mutability: None,
                                expr: Expr::Field {
                                    attrs: [],
                                    base: Expr::Path {
                                        attrs: [],
                                        qself: None,
                                        path: Path {
                                            leading_colon: None,
                                            segments: [
                                                PathSegment {
                                                    ident: Ident(
                                                        impl_a,
                                                    ),
                                                    arguments: PathArguments::None,
                                                },
                                            ],
                                        },
                                    },
                                    dot_token: Dot,
                                    member: Member::Named(
                                        Ident(
                                            binder,
                                        ),
                                    ),
                                },
                            },
                        ],
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_for_all() {
    let ast: ast::Premise = syn::parse2(quote! {
        (for_all(field in fields)
            (let Field { ty, .. } = field))
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: ForAll(
                ForAll {
                    identifier: Ident(
                        field,
                    ),
                    collection: Expr::Path {
                        attrs: [],
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: [
                                PathSegment {
                                    ident: Ident(
                                        fields,
                                    ),
                                    arguments: PathArguments::None,
                                },
                            ],
                        },
                    },
                    state: [],
                    body: Premises {
                        premises: [
                            Premise {
                                kind: Let(
                                    Let {
                                        binding: Pattern {
                                            pattern: Pat::Struct {
                                                attrs: [],
                                                qself: None,
                                                path: Path {
                                                    leading_colon: None,
                                                    segments: [
                                                        PathSegment {
                                                            ident: Ident(
                                                                Field,
                                                            ),
                                                            arguments: PathArguments::None,
                                                        },
                                                    ],
                                                },
                                                brace_token: Brace,
                                                fields: [
                                                    FieldPat {
                                                        attrs: [],
                                                        member: Member::Named(
                                                            Ident(
                                                                ty,
                                                            ),
                                                        ),
                                                        colon_token: None,
                                                        pat: Pat::Ident {
                                                            attrs: [],
                                                            by_ref: None,
                                                            mutability: None,
                                                            ident: Ident(
                                                                ty,
                                                            ),
                                                            subpat: None,
                                                        },
                                                    },
                                                    Comma,
                                                ],
                                                rest: Some(
                                                    PatRest {
                                                        attrs: [],
                                                        dot2_token: DotDot,
                                                    },
                                                ),
                                            },
                                        },
                                        expression: Expr::Path {
                                            attrs: [],
                                            qself: None,
                                            path: Path {
                                                leading_colon: None,
                                                segments: [
                                                    PathSegment {
                                                        ident: Ident(
                                                            field,
                                                        ),
                                                        arguments: PathArguments::None,
                                                    },
                                                ],
                                            },
                                        },
                                    },
                                ),
                                paren: Paren,
                                commit_point: None,
                            },
                        ],
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_for_all_with_state() {
    let ast: ast::Premise = syn::parse2(quote! {
        (for_all(field in fields with(state))
            (let Field { ty, .. } = field))
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: ForAll(
                ForAll {
                    identifier: Ident(
                        field,
                    ),
                    collection: Expr::Path {
                        attrs: [],
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: [
                                PathSegment {
                                    ident: Ident(
                                        fields,
                                    ),
                                    arguments: PathArguments::None,
                                },
                            ],
                        },
                    },
                    state: [
                        Ident(
                            state,
                        ),
                    ],
                    body: Premises {
                        premises: [
                            Premise {
                                kind: Let(
                                    Let {
                                        binding: Pattern {
                                            pattern: Pat::Struct {
                                                attrs: [],
                                                qself: None,
                                                path: Path {
                                                    leading_colon: None,
                                                    segments: [
                                                        PathSegment {
                                                            ident: Ident(
                                                                Field,
                                                            ),
                                                            arguments: PathArguments::None,
                                                        },
                                                    ],
                                                },
                                                brace_token: Brace,
                                                fields: [
                                                    FieldPat {
                                                        attrs: [],
                                                        member: Member::Named(
                                                            Ident(
                                                                ty,
                                                            ),
                                                        ),
                                                        colon_token: None,
                                                        pat: Pat::Ident {
                                                            attrs: [],
                                                            by_ref: None,
                                                            mutability: None,
                                                            ident: Ident(
                                                                ty,
                                                            ),
                                                            subpat: None,
                                                        },
                                                    },
                                                    Comma,
                                                ],
                                                rest: Some(
                                                    PatRest {
                                                        attrs: [],
                                                        dot2_token: DotDot,
                                                    },
                                                ),
                                            },
                                        },
                                        expression: Expr::Path {
                                            attrs: [],
                                            qself: None,
                                            path: Path {
                                                leading_colon: None,
                                                segments: [
                                                    PathSegment {
                                                        ident: Ident(
                                                            field,
                                                        ),
                                                        arguments: PathArguments::None,
                                                    },
                                                ],
                                            },
                                        },
                                    },
                                ),
                                paren: Paren,
                                commit_point: None,
                            },
                        ],
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_if_let() {
    let ast: ast::Premise = syn::parse2(quote! {
        (if let PlaceExpr::Var(id) = place)
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: IfLet(
                IfLet {
                    pattern: Pat::TupleStruct {
                        attrs: [],
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: [
                                PathSegment {
                                    ident: Ident(
                                        PlaceExpr,
                                    ),
                                    arguments: PathArguments::None,
                                },
                                PathSep,
                                PathSegment {
                                    ident: Ident(
                                        Var,
                                    ),
                                    arguments: PathArguments::None,
                                },
                            ],
                        },
                        paren_token: Paren,
                        elems: [
                            Pat::Ident {
                                attrs: [],
                                by_ref: None,
                                mutability: None,
                                ident: Ident(
                                    id,
                                ),
                                subpat: None,
                            },
                        ],
                    },
                    expression: Expr::Path {
                        attrs: [],
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: [
                                PathSegment {
                                    ident: Ident(
                                        place,
                                    ),
                                    arguments: PathArguments::None,
                                },
                            ],
                        },
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_comprehension_expression() {
    let ast: ast::Premise = syn::parse2(quote! {
        (let expected_names: Set<FieldName> = for_all field in fields => field.name)
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: Comprehension(
                Comprehesion {
                    binding: Identifier {
                        identifier: Ident(
                            expected_names,
                        ),
                        ty: Some(
                            Type::Path {
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                Set,
                                            ),
                                            arguments: PathArguments::AngleBracketed {
                                                colon2_token: None,
                                                lt_token: Lt,
                                                args: [
                                                    GenericArgument::Type(
                                                        Type::Path {
                                                            qself: None,
                                                            path: Path {
                                                                leading_colon: None,
                                                                segments: [
                                                                    PathSegment {
                                                                        ident: Ident(
                                                                            FieldName,
                                                                        ),
                                                                        arguments: PathArguments::None,
                                                                    },
                                                                ],
                                                            },
                                                        },
                                                    ),
                                                ],
                                                gt_token: Gt,
                                            },
                                        },
                                    ],
                                },
                            },
                        ),
                    },
                    loop_var: Ident(
                        field,
                    ),
                    collection: Expr::Path {
                        attrs: [],
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: [
                                PathSegment {
                                    ident: Ident(
                                        fields,
                                    ),
                                    arguments: PathArguments::None,
                                },
                            ],
                        },
                    },
                    body: ComprehensionBody {
                        premises: Premises {
                            premises: [],
                        },
                        conclusion: Expr::Field {
                            attrs: [],
                            base: Expr::Path {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                field,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                            },
                            dot_token: Dot,
                            member: Member::Named(
                                Ident(
                                    name,
                                ),
                            ),
                        },
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_comprehension_with_premises() {
    let ast: ast::Premise = syn::parse2(quote! {
        (let expected_names: Set<u32> = for_all field in fields =>
            (expected_field_name(field) => (a, _))
            a)
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: Comprehension(
                Comprehesion {
                    binding: Identifier {
                        identifier: Ident(
                            expected_names,
                        ),
                        ty: Some(
                            Type::Path {
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                Set,
                                            ),
                                            arguments: PathArguments::AngleBracketed {
                                                colon2_token: None,
                                                lt_token: Lt,
                                                args: [
                                                    GenericArgument::Type(
                                                        Type::Path {
                                                            qself: None,
                                                            path: Path {
                                                                leading_colon: None,
                                                                segments: [
                                                                    PathSegment {
                                                                        ident: Ident(
                                                                            u32,
                                                                        ),
                                                                        arguments: PathArguments::None,
                                                                    },
                                                                ],
                                                            },
                                                        },
                                                    ),
                                                ],
                                                gt_token: Gt,
                                            },
                                        },
                                    ],
                                },
                            },
                        ),
                    },
                    loop_var: Ident(
                        field,
                    ),
                    collection: Expr::Path {
                        attrs: [],
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: [
                                PathSegment {
                                    ident: Ident(
                                        fields,
                                    ),
                                    arguments: PathArguments::None,
                                },
                            ],
                        },
                    },
                    body: ComprehensionBody {
                        premises: Premises {
                            premises: [
                                Premise {
                                    kind: Implication(
                                        Implication {
                                            hypothesis: Expr::Call {
                                                attrs: [],
                                                func: Expr::Path {
                                                    attrs: [],
                                                    qself: None,
                                                    path: Path {
                                                        leading_colon: None,
                                                        segments: [
                                                            PathSegment {
                                                                ident: Ident(
                                                                    expected_field_name,
                                                                ),
                                                                arguments: PathArguments::None,
                                                            },
                                                        ],
                                                    },
                                                },
                                                paren_token: Paren,
                                                args: [
                                                    Expr::Path {
                                                        attrs: [],
                                                        qself: None,
                                                        path: Path {
                                                            leading_colon: None,
                                                            segments: [
                                                                PathSegment {
                                                                    ident: Ident(
                                                                        field,
                                                                    ),
                                                                    arguments: PathArguments::None,
                                                                },
                                                            ],
                                                        },
                                                    },
                                                ],
                                            },
                                            conclusion: Pat::Tuple {
                                                attrs: [],
                                                paren_token: Paren,
                                                elems: [
                                                    Pat::Ident {
                                                        attrs: [],
                                                        by_ref: None,
                                                        mutability: None,
                                                        ident: Ident(
                                                            a,
                                                        ),
                                                        subpat: None,
                                                    },
                                                    Comma,
                                                    Pat::Wild {
                                                        attrs: [],
                                                        underscore_token: Underscore,
                                                    },
                                                ],
                                            },
                                        },
                                    ),
                                    paren: Paren,
                                    commit_point: None,
                                },
                            ],
                        },
                        conclusion: Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            a,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_comprehension_with_premises2() {
    let ast: ast::Premise = syn::parse2(quote! {
        (let expected_names: Set<u32> = for_all field in fields =>
            (expected_field_name(field) => (a, _))
            (if a == "blub")
            a)
    })
    .unwrap();
    expect![[r#"
        Premise {
            kind: Comprehension(
                Comprehesion {
                    binding: Identifier {
                        identifier: Ident(
                            expected_names,
                        ),
                        ty: Some(
                            Type::Path {
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                Set,
                                            ),
                                            arguments: PathArguments::AngleBracketed {
                                                colon2_token: None,
                                                lt_token: Lt,
                                                args: [
                                                    GenericArgument::Type(
                                                        Type::Path {
                                                            qself: None,
                                                            path: Path {
                                                                leading_colon: None,
                                                                segments: [
                                                                    PathSegment {
                                                                        ident: Ident(
                                                                            u32,
                                                                        ),
                                                                        arguments: PathArguments::None,
                                                                    },
                                                                ],
                                                            },
                                                        },
                                                    ),
                                                ],
                                                gt_token: Gt,
                                            },
                                        },
                                    ],
                                },
                            },
                        ),
                    },
                    loop_var: Ident(
                        field,
                    ),
                    collection: Expr::Path {
                        attrs: [],
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: [
                                PathSegment {
                                    ident: Ident(
                                        fields,
                                    ),
                                    arguments: PathArguments::None,
                                },
                            ],
                        },
                    },
                    body: ComprehensionBody {
                        premises: Premises {
                            premises: [
                                Premise {
                                    kind: Implication(
                                        Implication {
                                            hypothesis: Expr::Call {
                                                attrs: [],
                                                func: Expr::Path {
                                                    attrs: [],
                                                    qself: None,
                                                    path: Path {
                                                        leading_colon: None,
                                                        segments: [
                                                            PathSegment {
                                                                ident: Ident(
                                                                    expected_field_name,
                                                                ),
                                                                arguments: PathArguments::None,
                                                            },
                                                        ],
                                                    },
                                                },
                                                paren_token: Paren,
                                                args: [
                                                    Expr::Path {
                                                        attrs: [],
                                                        qself: None,
                                                        path: Path {
                                                            leading_colon: None,
                                                            segments: [
                                                                PathSegment {
                                                                    ident: Ident(
                                                                        field,
                                                                    ),
                                                                    arguments: PathArguments::None,
                                                                },
                                                            ],
                                                        },
                                                    },
                                                ],
                                            },
                                            conclusion: Pat::Tuple {
                                                attrs: [],
                                                paren_token: Paren,
                                                elems: [
                                                    Pat::Ident {
                                                        attrs: [],
                                                        by_ref: None,
                                                        mutability: None,
                                                        ident: Ident(
                                                            a,
                                                        ),
                                                        subpat: None,
                                                    },
                                                    Comma,
                                                    Pat::Wild {
                                                        attrs: [],
                                                        underscore_token: Underscore,
                                                    },
                                                ],
                                            },
                                        },
                                    ),
                                    paren: Paren,
                                    commit_point: None,
                                },
                                Premise {
                                    kind: If(
                                        If {
                                            expression: Expr::Binary {
                                                attrs: [],
                                                left: Expr::Path {
                                                    attrs: [],
                                                    qself: None,
                                                    path: Path {
                                                        leading_colon: None,
                                                        segments: [
                                                            PathSegment {
                                                                ident: Ident(
                                                                    a,
                                                                ),
                                                                arguments: PathArguments::None,
                                                            },
                                                        ],
                                                    },
                                                },
                                                op: BinOp::Eq(
                                                    EqEq,
                                                ),
                                                right: Expr::Lit {
                                                    attrs: [],
                                                    lit: Lit::Str {
                                                        token: "blub",
                                                    },
                                                },
                                            },
                                        },
                                    ),
                                    paren: Paren,
                                    commit_point: None,
                                },
                            ],
                        },
                        conclusion: Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            a,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_fail_let_for_all1() {
    let err = syn::parse2::<ast::Comprehesion>(quote! {
        let result: Vec<u32> = for_all value in values =>
            (double(value)) * 3
    })
    .unwrap_err();
    expect![[r#"
        Error(
            "expected `=`",
        )
    "#]]
    .assert_debug_eq(&err);
}

#[test]
fn parse_fail_let_for_all2() {
    let err = syn::parse2::<ast::Comprehesion>(quote! {
        let result: Vec<u32> = for_all value in values =>
            (if *value > 0)
    })
    .unwrap_err();
    expect![[r#"
        Error(
            "unexpected end of input, expected an expression",
        )
    "#]]
    .assert_debug_eq(&err);
}

#[test]
fn parse_scope_with_empty_state() {
    let ast: ast::Premise = syn::parse2(quote! {
        (scope(enter_universally(env, &binder.explicit_binder) => (env, TraitBoundData { where_clauses, trait_items })) with())
    }).unwrap();
    expect![[r#"
        Premise {
            kind: Scope(
                Scope {
                    expression: Expr::Call {
                        attrs: [],
                        func: Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            enter_universally,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                        paren_token: Paren,
                        args: [
                            Expr::Path {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                env,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                            },
                            Comma,
                            Expr::Reference {
                                attrs: [],
                                and_token: And,
                                mutability: None,
                                expr: Expr::Field {
                                    attrs: [],
                                    base: Expr::Path {
                                        attrs: [],
                                        qself: None,
                                        path: Path {
                                            leading_colon: None,
                                            segments: [
                                                PathSegment {
                                                    ident: Ident(
                                                        binder,
                                                    ),
                                                    arguments: PathArguments::None,
                                                },
                                            ],
                                        },
                                    },
                                    dot_token: Dot,
                                    member: Member::Named(
                                        Ident(
                                            explicit_binder,
                                        ),
                                    ),
                                },
                            },
                        ],
                    },
                    inside: Pat::Tuple {
                        attrs: [],
                        paren_token: Paren,
                        elems: [
                            Pat::Ident {
                                attrs: [],
                                by_ref: None,
                                mutability: None,
                                ident: Ident(
                                    env,
                                ),
                                subpat: None,
                            },
                            Comma,
                            Pat::Struct {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                TraitBoundData,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                                brace_token: Brace,
                                fields: [
                                    FieldPat {
                                        attrs: [],
                                        member: Member::Named(
                                            Ident(
                                                where_clauses,
                                            ),
                                        ),
                                        colon_token: None,
                                        pat: Pat::Ident {
                                            attrs: [],
                                            by_ref: None,
                                            mutability: None,
                                            ident: Ident(
                                                where_clauses,
                                            ),
                                            subpat: None,
                                        },
                                    },
                                    Comma,
                                    FieldPat {
                                        attrs: [],
                                        member: Member::Named(
                                            Ident(
                                                trait_items,
                                            ),
                                        ),
                                        colon_token: None,
                                        pat: Pat::Ident {
                                            attrs: [],
                                            by_ref: None,
                                            mutability: None,
                                            ident: Ident(
                                                trait_items,
                                            ),
                                            subpat: None,
                                        },
                                    },
                                ],
                                rest: None,
                            },
                        ],
                    },
                    state: [],
                    body: Premises {
                        premises: [],
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_scope_with_state() {
    let ast: ast::Premise = syn::parse2(quote! {
        (scope(enter_universally(env, &binder.explicit_binder) => (env, TraitBoundData { where_clauses, trait_items })) with(a_vec))
    }).unwrap();
    expect![[r#"
        Premise {
            kind: Scope(
                Scope {
                    expression: Expr::Call {
                        attrs: [],
                        func: Expr::Path {
                            attrs: [],
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: [
                                    PathSegment {
                                        ident: Ident(
                                            enter_universally,
                                        ),
                                        arguments: PathArguments::None,
                                    },
                                ],
                            },
                        },
                        paren_token: Paren,
                        args: [
                            Expr::Path {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                env,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                            },
                            Comma,
                            Expr::Reference {
                                attrs: [],
                                and_token: And,
                                mutability: None,
                                expr: Expr::Field {
                                    attrs: [],
                                    base: Expr::Path {
                                        attrs: [],
                                        qself: None,
                                        path: Path {
                                            leading_colon: None,
                                            segments: [
                                                PathSegment {
                                                    ident: Ident(
                                                        binder,
                                                    ),
                                                    arguments: PathArguments::None,
                                                },
                                            ],
                                        },
                                    },
                                    dot_token: Dot,
                                    member: Member::Named(
                                        Ident(
                                            explicit_binder,
                                        ),
                                    ),
                                },
                            },
                        ],
                    },
                    inside: Pat::Tuple {
                        attrs: [],
                        paren_token: Paren,
                        elems: [
                            Pat::Ident {
                                attrs: [],
                                by_ref: None,
                                mutability: None,
                                ident: Ident(
                                    env,
                                ),
                                subpat: None,
                            },
                            Comma,
                            Pat::Struct {
                                attrs: [],
                                qself: None,
                                path: Path {
                                    leading_colon: None,
                                    segments: [
                                        PathSegment {
                                            ident: Ident(
                                                TraitBoundData,
                                            ),
                                            arguments: PathArguments::None,
                                        },
                                    ],
                                },
                                brace_token: Brace,
                                fields: [
                                    FieldPat {
                                        attrs: [],
                                        member: Member::Named(
                                            Ident(
                                                where_clauses,
                                            ),
                                        ),
                                        colon_token: None,
                                        pat: Pat::Ident {
                                            attrs: [],
                                            by_ref: None,
                                            mutability: None,
                                            ident: Ident(
                                                where_clauses,
                                            ),
                                            subpat: None,
                                        },
                                    },
                                    Comma,
                                    FieldPat {
                                        attrs: [],
                                        member: Member::Named(
                                            Ident(
                                                trait_items,
                                            ),
                                        ),
                                        colon_token: None,
                                        pat: Pat::Ident {
                                            attrs: [],
                                            by_ref: None,
                                            mutability: None,
                                            ident: Ident(
                                                trait_items,
                                            ),
                                            subpat: None,
                                        },
                                    },
                                ],
                                rest: None,
                            },
                        ],
                    },
                    state: [
                        Ident(
                            a_vec,
                        ),
                    ],
                    body: Premises {
                        premises: [],
                    },
                },
            ),
            paren: Paren,
            commit_point: None,
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn codegen_let() {
    let pattern = syn::Pat::parse_multi_with_leading_vert
        .parse2(quote! {FnBoundData { input_args, output_ty, where_clauses, body }})
        .unwrap();
    let expression = syn::parse2(quote! {bound_data}).unwrap();
    let let_ = ast::Let {
        binding: ast::Binding::Pattern { pattern },
        expression,
    };
    let dummy = quote!(dummy stream);

    let s = render_block(codegen::emit_let(&let_, "let", DUMMY.clone()));
    expect![[r#"
        fn __dummy() {
            match ::formality_core::judgment::try_catch::<_>(|| Ok(bound_data)) {
                Ok(p) => {
                    let proof_tree = ::formality_core::judgment::ProofTree::leaf(
                        format!(
                            "{} = {p:?}", stringify!(FnBoundData { input_args, output_ty,
                            where_clauses, body })
                        ),
                    );
                    let FnBoundData { input_args, output_ty, where_clauses, body } = &p;
                    child_proof_trees.push(proof_tree);
                    { dummy }
                }
                Err(e) => {
                    let cause = e;
                    let file = file!();
                    let line = line!();
                    let column = column!();
                    if __matched {
                        tracing::debug!(
                            "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                            "let", cause = cause, file = file, line = line, column = column,
                        );
                        failed_rules
                            .insert(::formality_core::judgment::FailedRule {
                                rule_name: Some("let".to_string()),
                                file: file.to_string(),
                                line: line,
                                column: column,
                                cause,
                            });
                    } else {
                        tracing::trace!(
                            "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                            "let", cause = cause, file = file, line = line, column = column,
                        );
                    }
                }
            }
        }
    "#]]
    .assert_eq(&s);
}

#[test]
fn codegen_if_let() {
    let pattern = syn::Pat::parse_multi_with_leading_vert
        .parse2(quote! {Some(places_live_on_continue)})
        .unwrap();
    let expression = syn::parse2(quote! {state.live_after_continue(label)}).unwrap();
    let if_let = ast::IfLet {
        pattern,
        expression,
    };

    let s = render_block(codegen::emit_if_let(&if_let, "if let", DUMMY.clone()));
    expect![[r#"
        fn __dummy() {
            match ::formality_core::judgment::try_catch(|| Ok(
                state.live_after_continue(label),
            )) {
                Ok(value) => {
                    if let Some(places_live_on_continue) = &value {
                        { dummy }
                    } else {
                        let cause = ::formality_core::judgment::RuleFailureCause::IfLetDidNotMatch {
                            pattern: stringify!(Some(places_live_on_continue)).to_string(),
                            value: format!("{:?}", value),
                        };
                        let file = file!();
                        let line = line!();
                        let column = column!();
                        if __matched {
                            tracing::debug!(
                                "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                                "if let", cause = cause, file = file, line = line, column =
                                column,
                            );
                            failed_rules
                                .insert(::formality_core::judgment::FailedRule {
                                    rule_name: Some("if let".to_string()),
                                    file: file.to_string(),
                                    line: line,
                                    column: column,
                                    cause,
                                });
                        } else {
                            tracing::trace!(
                                "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                                "if let", cause = cause, file = file, line = line, column =
                                column,
                            );
                        }
                    }
                }
                Err(e) => {
                    let cause = e;
                    let file = file!();
                    let line = line!();
                    let column = column!();
                    if __matched {
                        tracing::debug!(
                            "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                            "if let", cause = cause, file = file, line = line, column = column,
                        );
                        failed_rules
                            .insert(::formality_core::judgment::FailedRule {
                                rule_name: Some("if let".to_string()),
                                file: file.to_string(),
                                line: line,
                                column: column,
                                cause,
                            });
                    } else {
                        tracing::trace!(
                            "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                            "if let", cause = cause, file = file, line = line, column = column,
                        );
                    }
                }
            }
        }
    "#]]
    .assert_eq(&s);
}

#[test]
fn codegen_if() {
    let expression = syn::parse2(quote! {state.scope_has_label(label)}).unwrap();
    let if_ = ast::If { expression };

    let s = render_block(codegen::emit_if(&if_, "if", DUMMY.clone()));
    expect![[r#"
        fn __dummy() {
            let arg0 = state;
            let arg1 = label;
            let if_then = ::formality_core::judgment::IfThen::new(
                stringify!(state.scope_has_label(label)),
                vec![
                    (stringify!(state), ::formality_core::judgment::DebugString::new(arg0),),
                    (stringify!(label), ::formality_core::judgment::DebugString::new(arg1),),
                ],
            );
            if { arg0(arg1) } {
                child_proof_trees
                    .push(::formality_core::judgment::ProofTree::leaf(format!("{if_then:?}")));
                { dummy }
            } else {
                let cause = ::formality_core::judgment::RuleFailureCause::IfFalse(if_then);
                let file = file!();
                let line = line!();
                let column = column!();
                if __matched {
                    tracing::debug!(
                        "rule {rn} failed because {cause} ({file}:{line}:{column})", rn = "if",
                        cause = cause, file = file, line = line, column = column,
                    );
                    failed_rules
                        .insert(::formality_core::judgment::FailedRule {
                            rule_name: Some("if".to_string()),
                            file: file.to_string(),
                            line: line,
                            column: column,
                            cause,
                        });
                } else {
                    tracing::trace!(
                        "rule {rn} failed because {cause} ({file}:{line}:{column})", rn = "if",
                        cause = cause, file = file, line = line, column = column,
                    );
                }
            }
        }
    "#]]
    .assert_eq(&s);
}

#[test]
fn codegen_for_all() {
    let identifier = syn::parse2(quote! {variant}).unwrap();
    let collection = syn::parse2(quote! {variants}).unwrap();
    let state = vec![];
    let body = Premises {
        premises: std::collections::VecDeque::from([ast::Premise {
            kind: ast::PremiseKind::If(ast::If {
                expression: syn::parse2(quote! { dummy_expression }).unwrap(),
            }),
            paren: syn::token::Paren::default(),
            commit_point: None,
        }]),
    };

    let for_all = ast::ForAll {
        identifier,
        collection,
        state,
        body,
    };

    let s = render_block(codegen::emit_for_all(&for_all, "for all", DUMMY.clone()));
    expect![[r#"
        fn __dummy() {
            {
                let collection = variants;
                let mut errored = false;
                let (file, line, column) = ::formality_core::respan!(
                    variants((file!(), line!(), column!()))
                );
                let mut loop_carried = ();
                let mut for_all_proof_trees = vec![];
                for variant in collection {
                    if errored {
                        break;
                    }
                    #[allow(unused)]
                    let () = &loop_carried.clone();
                    let attributes = vec![("item".to_string(), format!("{:?}", variant))];
                    let mut child_proof_trees = vec![];
                    let mut next_carried_and_proof = None;
                    let if_then = ::formality_core::judgment::IfThen::new(
                        stringify!(dummy_expression),
                        vec![],
                    );
                    if { dummy_expression } {
                        child_proof_trees
                            .push(
                                ::formality_core::judgment::ProofTree::leaf(
                                    format!("{if_then:?}"),
                                ),
                            );
                        next_carried_and_proof = Some(((), child_proof_trees.clone()));
                    } else {
                        let cause = ::formality_core::judgment::RuleFailureCause::IfFalse(
                            if_then,
                        );
                        let file = file!();
                        let line = line!();
                        let column = column!();
                        if __matched {
                            tracing::debug!(
                                "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                                "for all", cause = cause, file = file, line = line, column =
                                column,
                            );
                            failed_rules
                                .insert(::formality_core::judgment::FailedRule {
                                    rule_name: Some("for all".to_string()),
                                    file: file.to_string(),
                                    line: line,
                                    column: column,
                                    cause,
                                });
                        } else {
                            tracing::trace!(
                                "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                                "for all", cause = cause, file = file, line = line, column =
                                column,
                            );
                        }
                    }
                    if let Some((next_carried, next_proof)) = next_carried_and_proof {
                        for_all_proof_trees
                            .push(
                                ::formality_core::judgment::ProofTree::with_all(
                                    "for_all",
                                    attributes,
                                    None,
                                    &file[..],
                                    line,
                                    column,
                                    next_proof,
                                ),
                            );
                        loop_carried = next_carried;
                    } else {
                        errored = true;
                    }
                }
                if !errored {
                    #[allow(unused)]
                    let () = &loop_carried.clone();
                    let for_all_tree = ::formality_core::judgment::ProofTree::new(
                        "for_all",
                        None,
                        for_all_proof_trees,
                    );
                    child_proof_trees.push(for_all_tree);
                    { dummy }
                }
            }
        }
    "#]]
    .assert_eq(&s);
}

#[test]
fn codegen_fail() {
    let expressions = vec![syn::parse2(quote! {true == false}).unwrap()];
    let message = syn::parse2(quote! { "an error {}" }).unwrap();
    let fail = ast::Fail {
        message,
        expressions,
    };

    let s = render_block(codegen::emit_fail(&fail, "fail", DUMMY.clone()));
    expect![[r#"
        fn __dummy() {
            {
                #[allow(unused_variables)]
                let _ = &child_proof_trees;
                let message = format!("an error {}", true == false);
                #[allow(unused_assignments)]
                {
                    __matched = true;
                }
                let cause = ::formality_core::judgment::RuleFailureCause::ExplicitFailure {
                    message,
                };
                let file = file!();
                let line = line!();
                let column = column!();
                if __matched {
                    tracing::debug!(
                        "rule {rn} failed because {cause} ({file}:{line}:{column})", rn = "fail",
                        cause = cause, file = file, line = line, column = column,
                    );
                    failed_rules
                        .insert(::formality_core::judgment::FailedRule {
                            rule_name: Some("fail".to_string()),
                            file: file.to_string(),
                            line: line,
                            column: column,
                            cause,
                        });
                } else {
                    tracing::trace!(
                        "rule {rn} failed because {cause} ({file}:{line}:{column})", rn = "fail",
                        cause = cause, file = file, line = line, column = column,
                    );
                }
            }
        }
    "#]]
    .assert_eq(&s);
}

#[test]
fn codegen_implication() {
    let hypothesis = syn::parse2(quote! {check_adt_variant_names_unique(adt)}).unwrap();
    let conclusion = syn::Pat::parse_multi_with_leading_vert
        .parse2(quote! { () })
        .unwrap();

    let implication = ast::Implication {
        hypothesis,
        conclusion,
    };

    let s = render_block(codegen::emit_implication(
        &implication,
        "implication",
        DUMMY.clone(),
    ));
    expect![[r#"
        fn __dummy() {
            match ::formality_core::judgment::CheckProven::check_proven(
                check_adt_variant_names_unique(adt),
            ) {
                Ok(proof_tree) => {
                    child_proof_trees.push(proof_tree);
                    { dummy }
                }
                Err(e) => {
                    let cause = e;
                    let file = file!();
                    let line = line!();
                    let column = column!();
                    if __matched {
                        tracing::debug!(
                            "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                            "implication", cause = cause, file = file, line = line, column =
                            column,
                        );
                        failed_rules
                            .insert(::formality_core::judgment::FailedRule {
                                rule_name: Some("implication".to_string()),
                                file: file.to_string(),
                                line: line,
                                column: column,
                                cause,
                            });
                    } else {
                        tracing::trace!(
                            "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                            "implication", cause = cause, file = file, line = line, column =
                            column,
                        );
                    }
                }
            }
        }
    "#]]
    .assert_eq(&s);
}

#[test]
fn codegen_in() {
    let expression = syn::parse2(
        quote! {a.where_clauses.iter().chain(&b.where_clauses).flat_map(|wc| wc.invert())},
    )
    .unwrap();
    let pattern = syn::Pat::parse_multi_with_leading_vert
        .parse2(quote! { wc })
        .unwrap();

    let in_ = ast::In {
        expression,
        pattern,
    };

    let s = render_block(codegen::emit_in(&in_, "in", DUMMY.clone()));
    expect![[r#"
        fn __dummy() {
            if let Err(e) = ::formality_core::judgment::member_of(
                a.where_clauses.iter().chain(&b.where_clauses).flat_map(|wc| wc.invert()),
                || {
                    stringify!(
                        a.where_clauses.iter().chain(& b.where_clauses).flat_map(| wc | wc
                        .invert())
                    )
                        .to_string()
                },
                |(value, proof_tree)| {
                    let wc = &value;
                    let len = child_proof_trees.len();
                    child_proof_trees.push(proof_tree);
                    { dummy }
                    assert!(child_proof_trees.len() > len);
                    child_proof_trees.truncate(len);
                },
            ) {
                let cause = e;
                let file = file!();
                let line = line!();
                let column = column!();
                if __matched {
                    tracing::debug!(
                        "rule {rn} failed because {cause} ({file}:{line}:{column})", rn = "in",
                        cause = cause, file = file, line = line, column = column,
                    );
                    failed_rules
                        .insert(::formality_core::judgment::FailedRule {
                            rule_name: Some("in".to_string()),
                            file: file.to_string(),
                            line: line,
                            column: column,
                            cause,
                        });
                } else {
                    tracing::trace!(
                        "rule {rn} failed because {cause} ({file}:{line}:{column})", rn = "in",
                        cause = cause, file = file, line = line, column = column,
                    );
                }
            }
        }
    "#]]
    .assert_eq(&s);
}

#[test]
fn codegen_scope() {
    let expression =
        syn::parse2(quote! { enter_universally(env, &binder.explicit_binder) }).unwrap();
    let pattern = syn::Pat::parse_multi_with_leading_vert
        .parse2(quote! { (env, TraitBoundData { where_clauses, trait_items }) })
        .unwrap();

    let state = vec![];
    let body = Premises {
        premises: std::collections::VecDeque::from([ast::Premise {
            kind: ast::PremiseKind::If(ast::If {
                expression: syn::parse2(quote! { dummy_expression }).unwrap(),
            }),
            paren: syn::token::Paren::default(),
            commit_point: None,
        }]),
    };

    let scope = ast::Scope {
        expression,
        inside: pattern,
        state,
        body,
    };

    let s = render_block(codegen::emit_scope(&scope, "scope", DUMMY.clone()));
    expect![[r#"
        fn __dummy() {
            match ::formality_core::judgment::try_catch(|| Ok(
                enter_universally(env, &binder.explicit_binder),
            )) {
                Err(e) => {
                    let cause = e;
                    let file = file!();
                    let line = line!();
                    let column = column!();
                    if __matched {
                        tracing::debug!(
                            "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                            "scope", cause = cause, file = file, line = line, column = column,
                        );
                        failed_rules
                            .insert(::formality_core::judgment::FailedRule {
                                rule_name: Some("scope".to_string()),
                                file: file.to_string(),
                                line: line,
                                column: column,
                                cause,
                            });
                    } else {
                        tracing::trace!(
                            "rule {rn} failed because {cause} ({file}:{line}:{column})", rn =
                            "scope", cause = cause, file = file, line = line, column = column,
                        );
                    }
                }
                Ok((scope, inside)) => {
                    let mut scope_proofs = vec![];
                    {
                        #[allow(unused_variables)]
                        let (env, TraitBoundData { where_clauses, trait_items }) = &inside;
                        #[allow(unused_mut)]
                        let mut child_proof_trees: Vec<::formality_core::judgment::ProofTree> = Vec::new();
                        let if_then = ::formality_core::judgment::IfThen::new(
                            stringify!(dummy_expression),
                            vec![],
                        );
                        if { dummy_expression } {
                            child_proof_trees
                                .push(
                                    ::formality_core::judgment::ProofTree::leaf(
                                        format!("{if_then:?}"),
                                    ),
                                );
                            scope_proofs.push(((), child_proof_trees.clone()));
                        } else {
                            let cause = ::formality_core::judgment::RuleFailureCause::IfFalse(
                                if_then,
                            );
                            let file = file!();
                            let line = line!();
                            let column = column!();
                            if __matched {
                                tracing::debug!(
                                    "rule {rn} failed because {cause} ({file}:{line}:{column})",
                                    rn = "scope", cause = cause, file = file, line = line, column
                                    = column,
                                );
                                failed_rules
                                    .insert(::formality_core::judgment::FailedRule {
                                        rule_name: Some("scope".to_string()),
                                        file: file.to_string(),
                                        line: line,
                                        column: column,
                                        cause,
                                    });
                            } else {
                                tracing::trace!(
                                    "rule {rn} failed because {cause} ({file}:{line}:{column})",
                                    rn = "scope", cause = cause, file = file, line = line, column
                                    = column,
                                );
                            }
                        }
                    }
                    for (values, inner_proof_trees) in scope_proofs {
                        if let Err(e) = ::formality_core::judgment::EachProof::each_proof(
                            ::formality_core::judgment::Scope::leave(&scope, values),
                            |(values, leave_proof_tree)| {
                                #[allow(unused_variables)]
                                let () = &values;
                                let len = child_proof_trees.len();
                                let mut scope_proof_trees = inner_proof_trees.clone();
                                scope_proof_trees.push(leave_proof_tree);
                                child_proof_trees
                                    .push(
                                        ::formality_core::judgment::ProofTree::new(
                                            "scope",
                                            None,
                                            scope_proof_trees,
                                        ),
                                    );
                                { dummy }
                                assert!(child_proof_trees.len() > len);
                                child_proof_trees.truncate(len);
                            },
                        ) {
                            let cause = e;
                            let file = file!();
                            let line = line!();
                            let column = column!();
                            if __matched {
                                tracing::debug!(
                                    "rule {rn} failed because {cause} ({file}:{line}:{column})",
                                    rn = "scope", cause = cause, file = file, line = line, column
                                    = column,
                                );
                                failed_rules
                                    .insert(::formality_core::judgment::FailedRule {
                                        rule_name: Some("scope".to_string()),
                                        file: file.to_string(),
                                        line: line,
                                        column: column,
                                        cause,
                                    });
                            } else {
                                tracing::trace!(
                                    "rule {rn} failed because {cause} ({file}:{line}:{column})",
                                    rn = "scope", cause = cause, file = file, line = line, column
                                    = column,
                                );
                            }
                        }
                    }
                }
            }
        }
    "#]].assert_eq(&s);
}
