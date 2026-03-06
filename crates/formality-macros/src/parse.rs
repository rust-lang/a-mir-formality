extern crate proc_macro;

use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, Attribute};
use synstructure::BindingInfo;

use crate::{
    attrs::{has_cast_attr, has_variable_attr, precedence, variable},
    spec::{self, FieldMode, FormalitySpec},
    variable::Variable,
};

/// Derive the `Parse` impl, using an optional grammar supplied "from the outside".
/// This is used by the `#[term(G)]` macro, which supplies the grammar `G`.
pub(crate) fn derive_parse_with_spec(
    mut s: synstructure::Structure,
    external_spec: Option<&FormalitySpec>,
) -> syn::Result<TokenStream> {
    s.underscore_const(true);

    if let syn::Data::Union(v) = &s.ast().data {
        return Err(syn::Error::new(
            v.union_token.span,
            "unions are not supported",
        ));
    }

    if external_spec.is_some() {
        // Only allow external specs for structs.
        match s.ast().data {
            syn::Data::Struct(_) => {}
            syn::Data::Enum(_) | syn::Data::Union(_) => {
                return Err(syn::Error::new_spanned(
                    &s.ast().ident,
                    "for enums provide the grammar on each variant".to_string(),
                ));
            }
        }
    }

    // Emit `#[variable]` variants first. If any succeeds, short-circuit
    // and skip remaining variants. This ensures that in-scope variables
    // are always preferred over identifiers (e.g., `T` as a type variable
    // vs `T` as an ADT name) without needing global disambiguation hacks.
    let mut variable_variants = TokenStream::new();
    let mut other_variants = TokenStream::new();
    for variant in s.variants() {
        let variant_name = Literal::string(&format!("{}::{}", s.ast().ident, variant.ast().ident));
        let v = parse_variant(variant, external_spec)?;
        let precedence = precedence(variant.ast().attrs)?.expr();
        let is_variable = has_variable_attr(variant.ast().attrs);
        let target = if is_variable {
            &mut variable_variants
        } else {
            &mut other_variants
        };
        target.extend(quote_spanned!(
            variant.ast().ident.span() =>
            __parser.parse_variant(#variant_name, #precedence, |__p| { #v });
        ));
    }
    let parse_variants = if variable_variants.is_empty() {
        other_variants
    } else {
        quote! {
            #variable_variants
            if !__parser.has_success() {
                #other_variants
            }
        }
    };

    let type_name: Literal = as_literal(&s.ast().ident);
    Ok(s.gen_impl(quote! {
        use formality_core::parse;

        gen impl parse::CoreParse<crate::FormalityLang> for @Self {
            fn parse<'t>(scope: &parse::Scope<crate::FormalityLang>, text: &'t str) -> parse::ParseResult<'t, Self>
            {
                parse::Parser::multi_variant(scope, text, #type_name, |__parser| {
                    #parse_variants;
                })
            }
        }
    }))
}

fn parse_variant(
    variant: &synstructure::VariantInfo,
    external_spec: Option<&FormalitySpec>,
) -> syn::Result<TokenStream> {
    let ast = variant.ast();
    let variable_attr = variable(ast.attrs)?;
    let mut stream = TokenStream::default();

    // When invoked like `#[term(foo)]`, use the spec from `foo`
    if let Some(spec) = external_spec {
        return parse_variant_with_attr(variant, spec, stream);
    }

    // Else, look for a `#[grammar]` attribute on the variant
    if let Some(attr) = get_grammar_attr(ast.attrs) {
        return parse_variant_with_attr(variant, &attr?, stream);
    }

    // If no `#[grammar(...)]` attribute is provided, then we provide default behavior.

    if variant.bindings().is_empty() {
        // No bindings (e.g., `Foo`) -- just parse a keyword `foo`
        let literal = Literal::string(&to_parse_ident(ast.ident));
        let construct = variant.construct(|_, _| quote! {});
        stream.extend(quote_spanned! {
            ast.ident.span() =>
            __p.expect_keyword(#literal)?;
            __p.ok(#construct)
        });
    } else if let Some(Variable { kind }) = variable_attr {
        // Has the `#[variable]` attribute -- parse an identifier and then check to see if it is present
        // in the scope. If so, downcast it and check that it has the correct kind.
        if variant.bindings().len() != 1 {
            return Err(syn::Error::new(
                variant.ast().ident.span(),
                "can only automatically parse variable variants with one binding",
            ));
        }
        let v0_binding = &variant.bindings()[0];
        let v0 = field_ident(v0_binding.ast(), 0);
        let construct = variant.construct(field_ident);
        stream.extend(quote_spanned! {
            ast.ident.span() =>
            let #v0 = __p.variable_of_kind(#kind)?;
            __p.ok(#construct)
        });
    } else if has_cast_attr(variant.ast().attrs) {
        // Has the `#[cast]` attribute -- just parse the bindings (comma separated, if needed)
        let construct = variant.construct(field_ident);
        let tail = quote_spanned! {
            ast.ident.span() =>
            __p.ok(#construct)
        };
        let body = wrap_bindings_in_each_nonterminal(variant.bindings(), tail);
        stream.extend(quote_spanned! {
            ast.ident.span() =>
            #body
        });
    } else {
        // Otherwise -- parse `variant(binding0, ..., bindingN)`
        let literal = Literal::string(&to_parse_ident(ast.ident));
        let construct = variant.construct(field_ident);
        let tail = quote_spanned! {
            ast.ident.span() =>
            __p.skip_trailing_comma();
            __p.expect_char(')')?;
            __p.ok(#construct)
        };
        let body = wrap_bindings_in_each_nonterminal(variant.bindings(), tail);
        stream.extend(quote_spanned! {
            ast.ident.span() =>
            __p.expect_keyword(#literal)?;
            __p.expect_char('(')?;
            #body
        });
    }

    Ok(stream)
}

/// When a type is given a formality attribute, we use that to guide parsing:
///
/// ```rust,ignore
/// #[formality(impl $data)]
/// struct Impl {
///     data: Binder<ImplBound>
/// }
///
/// #[formality($trait_id < $*tys > for $self_ty where $,where_clauses { $*trait_items })]
/// struct ImplBound {
///     trait_id: TraitId,
///     tys: Vec<Ty>,
///     self_ty: Ty,
///     where_clauses: Vec<WhereClause>,
/// }
/// ```
fn parse_variant_with_attr(
    variant: &synstructure::VariantInfo,
    spec: &FormalitySpec,
    mut stream: TokenStream,
) -> syn::Result<TokenStream> {
    // If the user added a commit point, then clear the committed flag initially.
    // It will be set back to true once we reach that commit point.
    if spec
        .symbols
        .iter()
        .any(|s| matches!(s, spec::FormalitySpecSymbol::CommitPoint))
    {
        stream.extend(quote!(__p.set_committed(false);));
    }

    // Build the symbols list into a nested structure.
    // Non-nonterminal symbols (keywords, chars, commit points) stay as flat `?`-based code.
    // All nonterminal field modes become `each_*` continuation-passing calls,
    // nesting everything that follows inside the continuation closure.
    let c = variant.construct(field_ident);
    let tail = quote!(__p.ok(#c));

    let body = wrap_symbols_in_each_nonterminal(variant, &spec.symbols, tail);
    stream.extend(body);

    Ok(stream)
}

/// Recursively wrap a list of spec symbols, nesting `each_nonterminal` for `FieldMode::Single`
/// fields and keeping other operations flat.
///
/// `captured_names` tracks variable names bound by outer `each_nonterminal` calls.
/// These must be cloned inside inner closures since the closures are `Fn`.
fn wrap_symbols_in_each_nonterminal(
    variant: &synstructure::VariantInfo,
    symbols: &[spec::FormalitySpecSymbol],
    tail: TokenStream,
) -> TokenStream {
    wrap_symbols_inner(variant, symbols, tail, &[])
}

/// Look up a field's type in a variant by name.
/// For named fields, matches by name. For tuple fields (v0, v1, etc.),
/// matches by index.
fn field_type_by_name<'a>(
    variant: &'a synstructure::VariantInfo,
    name: &Ident,
) -> Option<&'a syn::Type> {
    // First try matching by name
    let by_name = variant
        .bindings()
        .iter()
        .find(|b| b.ast().ident.as_ref() == Some(name))
        .map(|b| &b.ast().ty);
    if by_name.is_some() {
        return by_name;
    }

    // For tuple struct fields, the spec uses names like v0, v1, ...
    let name_str = name.to_string();
    if let Some(idx_str) = name_str.strip_prefix('v') {
        if let Ok(idx) = idx_str.parse::<usize>() {
            let bindings = variant.bindings();
            if idx < bindings.len() && bindings[idx].ast().ident.is_none() {
                return Some(&bindings[idx].ast().ty);
            }
        }
    }

    None
}

fn wrap_symbols_inner(
    variant: &synstructure::VariantInfo,
    symbols: &[spec::FormalitySpecSymbol],
    tail: TokenStream,
    captured_names: &[Ident],
) -> TokenStream {
    if symbols.is_empty() {
        return tail;
    }

    let (first, rest) = symbols.split_first().unwrap();

    match first {
        spec::FormalitySpecSymbol::Field { name, mode } => {
            wrap_field_mode(variant, name, mode, rest, tail, captured_names)
        }

        spec::FormalitySpecSymbol::Keyword { ident } => {
            let literal = as_literal(ident);
            let inner = wrap_symbols_inner(variant, rest, tail, captured_names);
            quote_spanned!(ident.span() =>
                __p.expect_keyword(#literal)?;
                #inner
            )
        }

        spec::FormalitySpecSymbol::CommitPoint => {
            let inner = wrap_symbols_inner(variant, rest, tail, captured_names);
            quote!(
                let () = __p.set_committed(true);
                #inner
            )
        }

        spec::FormalitySpecSymbol::Char { punct } => {
            let literal = Literal::character(punct.as_char());
            let inner = wrap_symbols_inner(variant, rest, tail, captured_names);
            quote_spanned!(punct.span() =>
                __p.expect_char(#literal)?;
                #inner
            )
        }

        spec::FormalitySpecSymbol::Delimeter { text } => {
            let literal = Literal::character(*text);
            let inner = wrap_symbols_inner(variant, rest, tail, captured_names);
            quote_spanned!(literal.span() =>
                __p.expect_char(#literal)?;
                #inner
            )
        }
    }
}

/// Wraps a field parse into a continuation-passing `each_*` call that nests the
/// remaining symbols inside the closure. All field modes now propagate ambiguity.
fn wrap_field_mode(
    variant: &synstructure::VariantInfo,
    name: &Ident,
    mode: &FieldMode,
    rest_symbols: &[spec::FormalitySpecSymbol],
    tail: TokenStream,
    captured_names: &[Ident],
) -> TokenStream {
    let field_ty = field_type_by_name(variant, name);

    // Add this name to captured list for inner closures
    let mut new_captured = captured_names.to_vec();
    new_captured.push(name.clone());
    let inner = wrap_symbols_inner(variant, rest_symbols, tail, &new_captured);

    // Clone captured variables at the top of the closure body
    let clones: Vec<TokenStream> = captured_names
        .iter()
        .map(|n| quote!(let #n = #n.clone();))
        .collect();

    match mode {
        FieldMode::Single => {
            if let Some(ty) = field_ty {
                quote_spanned!(name.span() =>
                    __p.each_nonterminal(|#name: #ty, __p| {
                        #(#clones)*
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_nonterminal(|#name, __p| {
                        #(#clones)*
                        #inner
                    })
                )
            }
        }

        FieldMode::Optional => {
            if let Some(ty) = field_ty {
                quote_spanned!(name.span() =>
                    __p.each_opt_nonterminal(|#name: Option<#ty>, __p| {
                        let #name: #ty = #name.unwrap_or_default();
                        #(#clones)*
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_opt_nonterminal(|#name, __p| {
                        let #name = #name.unwrap_or_default();
                        #(#clones)*
                        #inner
                    })
                )
            }
        }

        FieldMode::Many => {
            if let Some(ty) = field_ty {
                quote_spanned!(name.span() =>
                    __p.each_many_nonterminal(|#name: #ty, __p| {
                        #(#clones)*
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_many_nonterminal(|#name, __p| {
                        #(#clones)*
                        #inner
                    })
                )
            }
        }

        FieldMode::DelimitedVec {
            open,
            optional,
            close,
        } => {
            let open = Literal::character(*open);
            let close = Literal::character(*close);
            if let Some(ty) = field_ty {
                quote_spanned!(name.span() =>
                    __p.each_delimited_nonterminal(#open, #optional, #close, |#name: #ty, __p| {
                        #(#clones)*
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_delimited_nonterminal(#open, #optional, #close, |#name, __p| {
                        #(#clones)*
                        #inner
                    })
                )
            }
        }

        FieldMode::Comma => {
            if let Some(ty) = field_ty {
                quote_spanned!(name.span() =>
                    __p.each_comma_nonterminal(|#name: #ty, __p| {
                        #(#clones)*
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_comma_nonterminal(|#name, __p| {
                        #(#clones)*
                        #inner
                    })
                )
            }
        }

        FieldMode::Guarded { guard, mode } => {
            let guard_keyword = as_literal(guard);
            match mode.as_ref() {
                FieldMode::Single => {
                    if let Some(ty) = field_ty {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_nonterminal(|#name: #ty, __p| {
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_nonterminal(|#name, __p| {
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    }
                }
                FieldMode::Optional => {
                    if let Some(ty) = field_ty {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_opt_nonterminal(|#name: Option<#ty>, __p| {
                                        let #name: #ty = #name.unwrap_or_default();
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_opt_nonterminal(|#name, __p| {
                                        let #name = #name.unwrap_or_default();
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    }
                }
                FieldMode::Many => {
                    if let Some(ty) = field_ty {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_many_nonterminal(|#name: #ty, __p| {
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_many_nonterminal(|#name, __p| {
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    }
                }
                FieldMode::Comma => {
                    if let Some(ty) = field_ty {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_comma_nonterminal(|#name: #ty, __p| {
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_comma_nonterminal(|#name, __p| {
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    }
                }
                FieldMode::DelimitedVec {
                    open,
                    optional,
                    close,
                } => {
                    let open = Literal::character(*open);
                    let close = Literal::character(*close);
                    if let Some(ty) = field_ty {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_delimited_nonterminal(#open, #optional, #close, |#name: #ty, __p| {
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_delimited_nonterminal(#open, #optional, #close, |#name, __p| {
                                        #(#clones)*
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
                                    #(#clones)*
                                    #inner
                                }
                            }
                        )
                    }
                }
                FieldMode::Guarded { .. } => {
                    // Nested guarded — unlikely but handle by falling back
                    panic!("nested Guarded modes are not supported");
                }
            }
        }
    }
}

fn get_grammar_attr(attrs: &[Attribute]) -> Option<syn::Result<FormalitySpec>> {
    let attr = attrs.iter().find(|a| a.path().is_ident("grammar"))?;
    Some(attr.parse_args())
}

fn as_literal(ident: &Ident) -> Literal {
    Literal::string(&ident.to_string())
}

/// Convert a name like `Foo` into the name we expect to parse (`foo`).
///
/// Ideally we'd do `snake_case` conversion but I can't figure out best library for that.
fn to_parse_ident(ident: &Ident) -> String {
    ident.to_string().to_case(Case::Snake)
}

fn field_ident(field: &syn::Field, index: usize) -> syn::Ident {
    match &field.ident {
        Some(field_name) => field_name.clone(),
        None => syn::Ident::new(&format!("v{}", index), field.span()),
    }
}

/// Wraps a sequence of bindings in nested `each_nonterminal` calls.
/// Each binding becomes an `each_nonterminal` layer, with commas between them.
/// The `tail` is the code that runs after all bindings are parsed.
fn wrap_bindings_in_each_nonterminal(bindings: &[BindingInfo], tail: TokenStream) -> TokenStream {
    wrap_bindings_inner(bindings, tail, 0, &[])
}

/// Helper that tracks binding index for comma insertion and captured names for cloning.
fn wrap_bindings_inner(
    bindings: &[BindingInfo],
    tail: TokenStream,
    current_index: usize,
    captured_names: &[Ident],
) -> TokenStream {
    if bindings.is_empty() {
        return tail;
    }

    let (first, rest) = bindings.split_first().unwrap();
    let name = field_ident(first.ast(), current_index);
    let field_ty = &first.ast().ty;

    // Add this name to captured list for inner closures
    let mut new_captured = captured_names.to_vec();
    new_captured.push(name.clone());
    let inner = wrap_bindings_inner(rest, tail, current_index + 1, &new_captured);

    // Clone captured variables at the top of the closure body
    let clones: Vec<TokenStream> = captured_names
        .iter()
        .map(|n| quote!(let #n = #n.clone();))
        .collect();

    let comma = if current_index > 0 {
        Some(quote_spanned!(
            name.span() =>
            __p.expect_char(',')?;
        ))
    } else {
        None
    };

    quote_spanned! {
        name.span() =>
        #comma
        __p.each_nonterminal(|#name: #field_ty, __p| {
            #(#clones)*
            #inner
        })
    }
}
