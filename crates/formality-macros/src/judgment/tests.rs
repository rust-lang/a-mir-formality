use crate::judgment::ast;

use super::parse::parse;

use expect_test::expect;
use quote::quote;

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
    let ast: ast::If = syn::parse2(quote! {
        if f == "this is a function"
    })
    .unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_fail() {
    let ast: ast::Fail = syn::parse2(quote! {
        fail "error"
    })
    .unwrap();
    expect![[r#"
        Fail {
            message: LitStr {
                token: "error",
            },
            expressions: [],
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_fail_with_expr() {
    let ast: ast::Fail = syn::parse2(quote! {
        fail "error {}", f
    })
    .unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_implication() {
    let ast: ast::Implication = syn::parse2(quote! {
        check_fn(program, Env::default(), Predicates::t(), f, crate_id) => ()
    })
    .unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_in() {
    let ast: ast::In = syn::parse2(quote! {
        wc in a.where_clauses.iter().chain(&b.where_clauses).flat_map(|wc| wc.invert())
    })
    .unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_let() {
    let ast: ast::Let = syn::parse2(quote! {
        let (env, a) = Env::default().instantiate_universally(&impl_a.binder)
    })
    .unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_for_all() {
    let ast: ast::ForAll = syn::parse2(quote! {
        for_all(field in fields)
            (let Field { ty, .. } = field)
    })
    .unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
#[ignore = "not implemented yet"]
fn parse_for_all_with_state() {
    let ast: ast::ForAll = syn::parse2(quote! {
        for_all(field in fields with(state))
            (let Field { ty, .. } = field)
    })
    .unwrap();
    expect![[r#""#]].assert_debug_eq(&ast);
}

#[test]
fn parse_if_let() {
    let ast: ast::IfLet = syn::parse2(quote! {
        if let PlaceExpr::Var(id) = place
    })
    .unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_let_for_all_single_expression() {
    let ast: ast::LetForAll = syn::parse2(quote! {
        let expected_names: Set<FieldName> = for_all field in fields => field.name
    })
    .unwrap();
    expect![[r#"
        LetForAll {
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
            expression: Expr::Path {
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_let_for_all_with_premises() {
    let ast: ast::LetForAll = syn::parse2(quote! {
        let expected_names: Set<u32> = for_all field in fields =>
            (expected_field_name(field) => (a, _))
            a
    })
    .unwrap();
    expect![[r#"
        LetForAll {
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
            expression: Expr::Path {
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_let_for_all_with_premises2() {
    let ast: ast::LetForAll = syn::parse2(quote! {
        let expected_names: Set<u32> = for_all field in fields =>
            (expected_field_name(field) => (a, _))
            (if a == "blub")
            a
    })
    .unwrap();
    expect![[r#"
        LetForAll {
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
            expression: Expr::Path {
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
        }
    "#]].assert_debug_eq(&ast);
}

#[test]
fn parse_fail_let_for_all1() {
    let err = syn::parse2::<ast::LetForAll>(quote! {
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
    let err = syn::parse2::<ast::LetForAll>(quote! {
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
    let ast: ast::Scope = syn::parse2(quote! {
        scope(enter_universally(env, &binder.explicit_binder) => (env, TraitBoundData { where_clauses, trait_items })) with()
    }).unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}

#[test]
fn parse_scope_with_state() {
    let ast: ast::Scope = syn::parse2(quote! {
        scope(enter_universally(env, &binder.explicit_binder) => (env, TraitBoundData { where_clauses, trait_items })) with(a_vec)
    }).unwrap();
    expect![[r#"
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
        }
    "#]]
    .assert_debug_eq(&ast);
}
