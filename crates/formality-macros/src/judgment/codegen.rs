use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Attribute, BinOp, Expr, ExprBinary, ExprUnary, Ident, Pat, UnOp};

use crate::judgment::ast::{
    Argument, Ast, Binding, ForAll, If, Implication, In, Judgment, Let, PremiseKind, Premises,
    Rule, Signature,
};

pub type Rust = TokenStream;

pub fn emit(judgment: Ast) -> Rust {
    let attributes = emit_attributes(&judgment.attributes);
    let signature_tokens = emit_signature(&judgment.signature);
    let judgment_struct = emit_judgment_struct(&judgment);
    let function_body = emit_function_body(&judgment);

    quote! {
        #attributes
        #signature_tokens {
            #judgment_struct

            #function_body
        }
    }
}

fn emit_attributes(attributes: &Vec<Attribute>) -> TokenStream {
    quote! {
        #(#attributes)*
    }
}

fn emit_signature(
    Signature {
        visibility,
        name,
        arguments,
        return_,
    }: &Signature,
) -> TokenStream {
    let args: Vec<_> = arguments
        .iter()
        .map(|Argument { name, ty }| quote! { #name: impl ::formality_core::Upcast<#ty> })
        .collect();

    quote! {
        #visibility fn #name(#(#args),*) -> ::formality_core::ProvenSet<#return_>
    }
}

fn emit_judgment_struct(judgment: &Judgment) -> TokenStream {
    let input_tys: Vec<_> = judgment.input_tys();
    let input_names = judgment.input_names();

    let name = judgment.name();
    let debug_input_names = judgment.debug_input_names();

    quote! {
        #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone)]
        struct __JudgmentStruct(#(#input_tys),*);

        ::formality_core::cast_impl!(__JudgmentStruct);

        impl std::fmt::Debug for __JudgmentStruct {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut f = fmt.debug_struct(stringify!(#name));
                let __JudgmentStruct(#(#input_names),*) = self;
                #(
                    f.field(stringify!(#debug_input_names), #debug_input_names);
                )*
                f.finish()
            }
        }
    }
}

fn emit_function_body(judgment: &Judgment) -> TokenStream {
    let input_name: Vec<_> = judgment.input_names();
    let input_ty: Vec<_> = judgment.input_tys();
    let output = judgment.output();
    let name = judgment.name();
    let debug_input_name = judgment.debug_input_names();

    let assertions = emit_assertions(judgment);
    let trivials = emit_trivials(judgment);
    let rules = emit_rules(judgment);

    quote! {
        #(let #input_name: #input_ty = ::formality_core::Upcast::upcast(#input_name);)*

        #assertions
        #trivials

        let mut failed_rules = ::formality_core::set![];
        let input = __JudgmentStruct(#(#input_name),*);
        let output = ::formality_core::fixed_point::fixed_point::<
            __JudgmentStruct,
        ::formality_core::Map<#output, ::formality_core::judgment::ProofTree>,
        >(
            // Tracing span:
            |input| {
                let __JudgmentStruct(#(#input_name),*) = input;
                tracing::debug_span!(
                    stringify!(#name),
                    #(?#debug_input_name),*
                )
            },

            // Stack:
            {
                thread_local! {
                    static R: ::formality_core::judgment::JudgmentStack<__JudgmentStruct, #output> = Default::default()
                }
                &R
            },

            // Input:
            input.clone(),

            // Default value:
            |_| Default::default(),

            // Next value:
            |input: __JudgmentStruct| {
                let mut output: ::formality_core::Map<#output, ::formality_core::judgment::ProofTree> = ::formality_core::Map::new();

                failed_rules.clear();

                #[allow(unused)]
                let input_string = format!("{:?}", input);
                #rules
                output
            },
        );

        if !output.is_empty() {
            ::formality_core::ProvenSet::proven(output)
        } else {
            ::formality_core::ProvenSet::failed_rules(
                &input,
                ::formality_core::judgment::FailureLocation::caller(),
                failed_rules,
            )
        }
    }
}

fn emit_assertions(judgment: &Judgment) -> TokenStream {
    let input_name: Vec<_> = judgment.input_names();
    let assert_expr: Vec<_> = judgment.assertions();
    quote! {
        {
            #(
                #[allow(unused_variables)]
                let #input_name = &#input_name;
            )*
            #(
                ::formality_core::respan!(
                    #assert_expr
                    (
                        ::formality_core::judgment::JudgmentAssertion::assert(#assert_expr, stringify!(#assert_expr));
                    )
                )
            )*
        }
    }
}

fn emit_trivials(judgment: &Judgment) -> TokenStream {
    let trivial_expr: Vec<_> = judgment.trivials_exprs();
    let trivial_result: Vec<_> = judgment.trivials_results();

    quote! {
        #(
            // Trivial cases are an (important) optimization that lets
            // you cut out all the normal rules.
            if #trivial_expr {
                let trivial_result = #trivial_result;
                let (file, line, column) = ::formality_core::respan!(
                    #trivial_expr
                    ((file!(), line!(), column!()))
                );
                let proof_tree = ::formality_core::judgment::ProofTree::with_all(
                    format!("trivial, as {} is true: {trivial_result:?}", stringify!(#trivial_expr)),
                    Default::default(),
                    None,
                    file,
                    line,
                    column,
                    Default::default(),
                );
                return ::formality_core::ProvenSet::singleton((trivial_result, proof_tree));
            }
        )*
    }
}

fn emit_rules(judgment: &Judgment) -> TokenStream {
    let rules: Vec<_> = judgment
        .body
        .rules
        .rules
        .iter()
        .map(|rule| emit_rule(judgment, rule))
        .collect();

    quote! {
        #(#rules)*
    }
}

fn emit_rule(judgment: &Judgment, rule: &Rule) -> TokenStream {
    let input_names = judgment.input_names();
    let judgment_name = judgment.name();
    let conclusion_name = rule.conclusion.name.clone();
    let rule_name = rule.name.clone();

    // TODO: For the moment always true.
    // Must think about when working with the commit points
    // __matched is updated, from true to false, if a commit point (!) is found.
    let match_default = true;

    let collect = emit_collect(judgment, rule);
    let premises = emit_premises(judgment, rule, &rule.premises, collect);

    // TODO: Maybe move to its own function
    let rule_stream = quote! {
        tracing::trace_span!("matched rule", rule = #rule_name, judgment = stringify!(#judgment_name)).in_scope(|| {
            tracing::debug!("matched rule {:?}", #rule_name);
            #[allow(unused_mut)]
            let mut child_proof_trees: Vec<::formality_core::judgment::ProofTree> = Vec::new();

            #premises
        });
    };
    let match_and_rule = emit_match(judgment, rule, rule_stream);

    quote! {
        {
            ::formality_macros::respan!(
                #conclusion_name
                (
                    // give the user a type error if the name they gave
                    // in the conclusion is not the same as the name of the
                    // function
                    #[allow(dead_code)]
                    struct WrongJudgmentNameInConclusion;
                    const _: WrongJudgmentNameInConclusion = {
                        let #judgment_name = WrongJudgmentNameInConclusion;
                        #conclusion_name
                    };
                )
            );

            if let Some(__JudgmentStruct(#(#input_names),*)) = Some(&input) {
                let __judgment_name = stringify!(#judgment_name);
                let __attributes: Vec<(String, String)> = vec![
                    #((stringify!(#input_names).to_string(), format!("{:?}", #input_names))),*
                ];
                let mut __matched = #match_default;
                #match_and_rule
            }
        }
    }
}

fn emit_match(judgment: &Judgment, rule: &Rule, rule_stream: TokenStream) -> TokenStream {
    rule.conclusion
        .patterns
        .iter()
        .zip(&judgment.signature.arguments)
        .rev()
        .fold(
            rule_stream,
            |acc, (pattern, Argument { name, ty })| match pattern {
                Pat::Path(expr_path) => {
                    quote! {
                        if let Some(#pattern) = &::formality_core::Downcast::downcast(#name) {
                            #acc
                        }
                    }
                }
                Pat::Struct(pat_struct) => todo!(),
                Pat::Tuple(pat_tuple) => todo!(),
                Pat::TupleStruct(pat_tuple_struct) => {
                    quote! {
                        if let Some(#pattern) = &::formality_core::Downcast::downcast(#name) {
                            #acc
                        }
                    }
                }

                Pat::Ident(pat_ident) => {
                    quote! { { let #pat_ident = #name; #acc}}
                }
                Pat::Const(expr_const) => todo!(),
                Pat::Lit(expr_lit) => todo!(),
                Pat::Macro(expr_macro) => todo!(),
                Pat::Or(pat_or) => todo!(),
                Pat::Paren(pat_paren) => todo!(),
                Pat::Range(expr_range) => todo!(),
                Pat::Reference(pat_reference) => todo!(),
                Pat::Rest(pat_rest) => todo!(),
                Pat::Slice(pat_slice) => todo!(),
                Pat::Type(pat_type) => todo!(),
                Pat::Verbatim(token_stream) => todo!(),
                Pat::Wild(pat_wild) => todo!(),
                _ => todo!(),
            },
        )
}

fn emit_premises(
    judgment: &Judgment,
    rule: &Rule,
    premises: &Premises,
    token_stream: TokenStream,
) -> TokenStream {
    premises
        .premises
        .iter()
        .rev()
        .fold(token_stream, |acc, premise| match &premise.kind {
            PremiseKind::IfLet(_) => todo!("if let"),
            PremiseKind::If(if_) => emit_if(judgment, rule, if_, acc),
            PremiseKind::ForAll(for_all) => emit_for_all(judgment, rule, for_all, acc),
            PremiseKind::Fail(fail) => todo!("fail"),
            PremiseKind::Implication(implication) => {
                emit_implication(judgment, rule, implication, acc)
            }
            PremiseKind::Let(let_) => emit_let(judgment, rule, let_, acc),
            PremiseKind::LetForAll(let_for_all) => todo!("let for all"),
            PremiseKind::In(in_) => emit_in(judgment, rule, in_, acc),
            PremiseKind::Scope(scope) => todo!("scope"),
        })
}

fn emit_if(judgment: &Judgment, rule: &Rule, if_: &If, token_stream: TokenStream) -> TokenStream {
    let origcond = &if_.expression;
    let record_failure = emit_record_failure(&rule.name);

    let (argn, arge, cond) = if_helper(&if_.expression);

    // TODO: Get the arguments for better error reporting
    quote! {
        #(
            let #argn = #arge;
        )*
        let if_then = ::formality_core::judgment::IfThen::new(
            stringify!(#origcond),
            vec![
                #(
                    (
                        stringify!(#arge),
                        ::formality_core::judgment::DebugString::new(#argn),
                    ),
                )*
            ],
        );
        if {
            #cond
        } {
            child_proof_trees.push(::formality_core::judgment::ProofTree::leaf(format!("{if_then:?}")));
            #token_stream
        } else {
            let cause = ::formality_core::judgment::RuleFailureCause::IfFalse(if_then);
            #record_failure
        }
    }
}

// TODO: Extend for more cases.
// Currently mirrors the bahvior of the existing macro_rule macro.
// This functions is also ugly as hell.
fn if_helper(expression: &Expr) -> (Vec<Ident>, Vec<&Expr>, TokenStream) {
    let mut argn = vec![];
    let mut arge = vec![];
    let mut cond = quote! { #expression };

    let arg0n = Ident::new("arg0", Span::call_site());
    let arg1n = Ident::new("arg1", Span::call_site());
    let arg2n = Ident::new("arg2", Span::call_site());

    match &expression {
        Expr::Call(expr_call) => {
            if expr_call.args.len() == 1 {
                let arg0e = &expr_call.args[0];
                arge.push(arg0e);
                argn.push(arg0n);
                cond = quote! { arg0(arg1) };
            } else if expr_call.args.len() == 2 {
                let arg0e = &expr_call.args[0];
                arge.push(arg0e);
                argn.push(arg0n);
                cond = quote! { arg0(arg1, arg2) };
            }
        }
        Expr::Binary(ExprBinary {
            attrs,
            left,
            op,
            right,
        }) => match (&op, &left.as_ref(), &right.as_ref()) {
            (BinOp::Eq(_), Expr::Path(_), Expr::Path(_)) => {
                let arg0 = left;
                let arg1 = right;
                arge.push(arg0);
                arge.push(arg1);
                argn.push(arg0n);
                argn.push(arg1n);
                cond = quote! { arg0 == arg1 }
            }
            (BinOp::Ne(_), Expr::Path(_), Expr::Path(_)) => {
                let arg0 = left;
                let arg1 = right;
                arge.push(arg0);
                arge.push(arg1);
                argn.push(arg0n);
                argn.push(arg1n);
                cond = quote! { arg0 != arg1 }
            }
            _ => {}
        },
        Expr::MethodCall(expr_method_call) => {
            let arg0e = &expr_method_call.receiver;
            argn.push(arg0n.clone());
            arge.push(arg0e);

            if expr_method_call.args.len() == 2 {
                let arg1e = &expr_method_call.args[0];
                let arg2e = &expr_method_call.args[1];
                argn.push(arg1n);
                argn.push(arg2n);
                arge.push(arg1e);
                arge.push(arg2e);
                cond = quote! { arg0(arg1, arg2) };
            } else if expr_method_call.args.len() == 1 {
                let arg1e = &expr_method_call.args[0];
                argn.push(arg1n);
                arge.push(arg1e);
                cond = quote! { arg0(arg1) };
            }
        }
        Expr::Unary(ExprUnary {
            attrs,
            op: UnOp::Not(_),
            expr,
        }) => {
            let (argnx, argex, condx) = if_helper(expr);
            argn = argnx;
            arge = argex;
            cond = quote! { !#cond };
        }
        _ => {}
    }

    (argn, arge, cond)
}

fn emit_for_all(
    judgment: &Judgment,
    rule: &Rule,
    for_all: &ForAll,
    token_stream: TokenStream,
) -> TokenStream {
    let collection = &for_all.collection;
    let loop_var = &for_all.identifier;
    let with_var = &for_all.state;

    let record_failure = emit_record_failure(&rule.name);
    let collect = quote! { next_carried_and_proof = Some(((#(#with_var.clone(),)*), child_proof_trees.clone())); };
    let body = emit_premises(judgment, rule, &for_all.body, collect);

    quote! {
        {
            let collection = #collection;
            let mut errored = false;

            let (file, line, column) = ::formality_core::respan!(#collection ((file!(), line!(), column!())));

            let mut loop_carried = (#(#with_var.clone(),)*);

            let mut for_all_proof_trees = vec![];
            for #loop_var in collection {
                if errored {
                    break;
                }

                #[allow(unused)]
                let (#(#with_var,)*) = &loop_carried.clone();

                let attributes = vec![("item".to_string(), format!("{:?}", #loop_var))];
                // let mut item_proof_trees = vec![];
                // I shadown the outer `child_proof_trees` delibertly, such that the generated code for the premises still uses the correct vector.
                let mut child_proof_trees = vec![];

                // Collect the proof tree from each iteration together with the
                // next values of any loop-carried variables, so the `for_all`
                // proof tree reflects all iterations (not just the last one).
                let mut next_carried_and_proof = None;
                // TODO: here is something missing
                // $crate::push_rules!(@body (loop(next_carried_and_proof, ($($with_var,)*))); $inputs; item_proof_trees; $($inner_step)*);
                #body

                if let Some((next_carried, next_proof)) = next_carried_and_proof {
                    for_all_proof_trees.push(::formality_core::judgment::ProofTree::with_all(
                        "for_all",
                        attributes,
                        None,
                        &file[..],
                        line,
                        column,
                        next_proof,
                    ));

                    loop_carried = next_carried;
                } else {
                    errored = true;
                }
            }

            if !errored {
                #[allow(unused)]
                let (#(#with_var,)*) = &loop_carried.clone();

                // All iterations succeeded
                let for_all_tree = ::formality_core::judgment::ProofTree::new(
                    "for_all",
                    None,
                    for_all_proof_trees,
                );
                child_proof_trees.push(for_all_tree);
                #token_stream
            }
        }
    }
}

fn emit_let(
    judgment: &Judgment,
    rule: &Rule,
    let_: &Let,
    token_stream: TokenStream,
) -> TokenStream {
    let record_failure = emit_record_failure(&rule.name);
    let expression = &let_.expression;
    let p = match &let_.binding {
        Binding::Pattern { pattern } => quote! { #pattern },
        Binding::Identifier { identifier, ty } => quote! { #identifier},
    };

    quote! {
        match ::formality_core::judgment::try_catch::<_>(|| Ok(#expression)) {
            Ok(p) => {
                let proof_tree = ::formality_core::judgment::ProofTree::leaf(format!("{} = {p:?}", stringify!(#p)));
                let #p = &p;
                child_proof_trees.push(proof_tree);
                #token_stream
            }

            Err(e) => {
                let cause = e;
                #record_failure
            }
        }
    }
}

fn emit_in(judgment: &Judgment, rule: &Rule, in_: &In, token_stream: TokenStream) -> TokenStream {
    let record_failure = emit_record_failure(&rule.name);

    let expression = &in_.expression;
    let pattern = &in_.pattern;
    quote! {
        if let Err(e) = ::formality_core::judgment::member_of(
            #expression,
            || stringify!(#expression).to_string(),
            |(value, proof_tree)| {
                let #pattern = &value;

                // Remember the size of the child proof tree stack
                let len = child_proof_trees.len();

                // Push this child proof tree
                child_proof_trees.push(proof_tree);

                // Recursively process successors
                #token_stream

                // Restore the original child proof tree stack; note that there may be multiple entries
                // in case "non-iterative" steps like `(let ...)` and `(if ...)` are interspersed.
                assert!(child_proof_trees.len() > len);
                child_proof_trees.truncate(len);
            },
        ) {
            let cause = e;
            #record_failure
        }
    }
}

fn emit_implication(
    judgment: &Judgment,
    rule: &Rule,
    implication: &Implication,
    token_stream: TokenStream,
) -> TokenStream {
    let record_failure = emit_record_failure(&rule.name);
    let i = &implication.hypothesis;

    quote! {
        match ::formality_core::judgment::CheckProven::check_proven(
            #i,
        ) {
            Ok(proof_tree) => {
                child_proof_trees.push(proof_tree);
                #token_stream
            }
            Err(e) => {
                let cause = e;
                #record_failure
            }
        }
    }
}

fn emit_record_failure(rule_name: &str) -> TokenStream {
    // failed_rules, __matched, cause are in scope
    // TODO: Update parser, such that the Premise span start with the opening paren and ends at the closing paren.
    let step_expr = quote! {};

    quote! {
        // TODO: Get somehow this respan correctly
        // let file = ::formality_core::respan!(#step_expr (file!()));
        // let line = ::formality_core::respan!(#step_expr (line!()));
        // let column = ::formality_core::respan!(#step_expr (column!()));
        let file = file!();
        let line = line!();
        let column = column!();

        if __matched {
            tracing::debug!(
                "rule {rn} failed because {cause} ({file}:{line}:{column})",
                rn = #rule_name,
                cause = cause,
                file = file,
                line = line,
                column = column,
            );
            failed_rules.insert(
                ::formality_core::judgment::FailedRule {
                    rule_name: Some(#rule_name.to_string()),
                    file: file.to_string(),
                    line: line,
                    column: column,
                    cause,
                }
            );
        } else {
            tracing::trace!(
                "rule {rn} failed because {cause} ({file}:{line}:{column})",
                rn = #rule_name,
                cause = cause,
                file = file,
                line = line,
                column = column,
            );
        }
    }
}

fn emit_collect(judgment: &Judgment, rule: &Rule) -> TokenStream {
    let judgment_name = judgment.name();
    let rule_name = &rule.name;
    let result = &rule.conclusion.result;

    quote! {
        {
            let result = ::formality_core::Upcast::upcast(#result);
            let mut attributes = __attributes.clone();
            attributes.push(("result".to_string(), format!("{:?}", result)));
            let (file, line, column) = ::formality_core::respan!(#rule_name ((file!(), line!(), column!())));
            let proof_tree = ::formality_core::judgment::ProofTree::with_all(
                __judgment_name,
                attributes,
                Some(#rule_name),
                file,
                line,
                column,
                child_proof_trees.clone(),
            );
            tracing::debug!("produced {:?} from rule {:?} in judgment {:?}", result, #rule_name, stringify!(#judgment_name));
            ::formality_core::judgment::insert_smallest_proof(&mut output, result, proof_tree);
        }
    }
}
