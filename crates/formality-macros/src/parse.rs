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

/// Derives a `CoreParse` impl for a struct or enum.
///
/// For each variant, generates code that tries to parse the input text and
/// returns all successful parses (ambiguity is propagated upward, not resolved here).
///
/// `#[variable]` variants are tried first and short-circuit on success, ensuring
/// in-scope variables are preferred over identifiers. For example, given:
///
/// ```rust,ignore
/// #[term]
/// pub enum TyData {
///     #[cast]
///     RigidTy(RigidTy),
///     #[variable(ParameterKind::Ty)]
///     Variable(Variable),
/// }
/// ```
///
/// If `T` is a type variable in scope, it parses as `Variable(T)` without
/// trying `RigidTy`.
///
/// The `external_spec` parameter is used by `#[term(grammar)]` on structs
/// to supply the grammar from outside rather than from a `#[grammar]` attribute.
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

/// Generates the parsing code for a single variant, choosing a strategy based on attributes.
///
/// There are four cases:
///
/// 1. **`#[grammar(...)]` or external spec** — parse according to the spec. See
///    [`parse_variant_with_attr`] for details.
///
/// 2. **`#[variable(kind)]`** — parse a variable reference from scope:
///    ```rust,ignore
///    #[variable(ParameterKind::Ty)]
///    Variable(Variable),
///    // input "T" (where T is in scope) => Variable(T)
///    ```
///
/// 3. **`#[cast]`** — transparent parse-through, no keyword prefix:
///    ```rust,ignore
///    #[cast]
///    RigidTy(RigidTy),
///    // input "u32" => RigidTy(RigidTy { name: u32, .. })
///    ```
///
/// 4. **Default** (no attributes, has fields) — parse as `snake_case_name(field0, field1, ...)`:
///    ```rust,ignore
///    FnCall(Expr, Vec<Expr>),
///    // input "fn_call(x, y)" => FnCall(x, y)
///    ```
///    Fieldless variants parse as just the keyword: `Foo` parses `"foo"`.
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
        let construct = variant.construct(field_ident_cloned);
        let tail = quote_spanned! {
            ast.ident.span() =>
            __p.ok(#construct)
        };
        let body = wrap_bindings(variant.bindings(), tail, 0);
        stream.extend(quote_spanned! {
            ast.ident.span() =>
            #body
        });
    } else {
        // Otherwise -- parse `variant(binding0, ..., bindingN)`
        let literal = Literal::string(&to_parse_ident(ast.ident));
        let construct = variant.construct(field_ident_cloned);
        let tail = quote_spanned! {
            ast.ident.span() =>
            __p.skip_trailing_comma();
            __p.expect_char(')')?;
            __p.ok(#construct)
        };
        let body = wrap_bindings(variant.bindings(), tail, 0);
        stream.extend(quote_spanned! {
            ast.ident.span() =>
            __p.expect_keyword(#literal)?;
            __p.expect_char('(')?;
            #body
        });
    }

    Ok(stream)
}

/// Generates parsing code driven by a `#[grammar(...)]` spec.
///
/// The spec is a sequence of symbols: keywords, characters, and field references
/// (prefixed with `$`). Each field reference becomes an `each_nonterminal` call
/// that forks execution for ambiguous parses. Keywords and characters are flat
/// `expect_*` calls.
///
/// For example:
///
/// ```rust,ignore
/// #[grammar(goto $,v0)]
/// Goto(Vec<BasicBlockId>),
/// // input "goto bb1, bb2" => Goto([bb1, bb2])
///
/// #[grammar($v0 = $v1;)]
/// Assign(PlaceExpression, ValueExpression),
/// // input "local(x) = constant(1: u32);" => Assign(local(x), constant(1: u32))
/// ```
///
/// The generated code nests closures from left to right. For `$v0 = $v1;`, this produces:
///
/// ```rust,ignore
/// __p.each_nonterminal(|v0: PlaceExpression, __p| {
///     __p.expect_char('=')?;
///     __p.each_nonterminal(|v1: ValueExpression, __p| {
///         __p.expect_char(';')?;
///         __p.ok(Assign(v0.clone(), v1.clone()))
///     })
/// })
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
    let c = variant.construct(field_ident_cloned);
    let tail = quote!(__p.ok(#c));

    let body = wrap_symbols(variant, &spec.symbols, tail);
    stream.extend(body);

    Ok(stream)
}

/// Looks up a field's type in a variant by name.
///
/// For named fields (e.g., `struct Foo { name: Ty }`), matches by name.
/// For tuple fields (e.g., `Bar(Ty, Ty)`), the spec uses synthetic names
/// `v0`, `v1`, etc., so this strips the `v` prefix and indexes by position.
///
/// Returns `None` if the field isn't found (the generated code omits
/// the type annotation and lets inference figure it out).
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

/// Recursively converts a list of grammar symbols into nested code.
///
/// Field symbols become `each_*` continuation-passing calls (one closure layer per field).
/// Everything else (keywords, characters, commit points) becomes flat `expect_*`/`set_committed`
/// code within the current closure. The `tail` is the innermost code that constructs the result.
///
/// For example, given symbols `[Keyword("for"), Field(v0, Single)]` and tail `__p.ok(ForAll(v0.clone()))`:
///
/// ```rust,ignore
/// __p.expect_keyword("for")?;        // flat: keyword
/// __p.each_nonterminal(|v0, __p| {   // nested: field
///     __p.ok(ForAll(v0.clone()))      // tail
/// })
/// ```
fn wrap_symbols(
    variant: &synstructure::VariantInfo,
    symbols: &[spec::FormalitySpecSymbol],
    tail: TokenStream,
) -> TokenStream {
    if symbols.is_empty() {
        return tail;
    }

    let (first, rest) = symbols.split_first().unwrap();

    match first {
        spec::FormalitySpecSymbol::Field { name, mode } => {
            wrap_field_mode(variant, name, mode, rest, tail)
        }

        spec::FormalitySpecSymbol::Keyword { ident } => {
            let literal = as_literal(ident);
            let inner = wrap_symbols(variant, rest, tail);
            quote_spanned!(ident.span() =>
                __p.expect_keyword(#literal)?;
                #inner
            )
        }

        spec::FormalitySpecSymbol::CommitPoint => {
            let inner = wrap_symbols(variant, rest, tail);
            quote!(
                let () = __p.set_committed(true);
                #inner
            )
        }

        spec::FormalitySpecSymbol::Char { punct } => {
            let literal = Literal::character(punct.as_char());
            let inner = wrap_symbols(variant, rest, tail);
            quote_spanned!(punct.span() =>
                __p.expect_char(#literal)?;
                #inner
            )
        }

        spec::FormalitySpecSymbol::Delimeter { text } => {
            let literal = Literal::character(*text);
            let inner = wrap_symbols(variant, rest, tail);
            quote_spanned!(literal.span() =>
                __p.expect_char(#literal)?;
                #inner
            )
        }
    }
}

/// Generates an `each_*` continuation call for a single field, nesting the remaining
/// symbols inside the closure body. The `each_*` method is chosen by the field mode:
///
/// | Spec syntax | Mode | Generated call |
/// |---|---|---|
/// | `$x` | `Single` | `each_nonterminal` |
/// | `$?x` | `Optional` | `each_opt_nonterminal` |
/// | `$*x` | `Many` | `each_many_nonterminal` |
/// | `$,x` | `Comma` | `each_comma_nonterminal` |
/// | `$<x>` | `DelimitedVec` | `each_delimited_nonterminal` |
/// | `$:guard $x` | `Guarded` | `expect_keyword` then `each_*`, or `Default` on miss |
///
/// The closures are `Fn` (called once per ambiguous child parse), so field values used
/// in the tail constructor are cloned via `field_ident_cloned`.
fn wrap_field_mode(
    variant: &synstructure::VariantInfo,
    name: &Ident,
    mode: &FieldMode,
    rest_symbols: &[spec::FormalitySpecSymbol],
    tail: TokenStream,
) -> TokenStream {
    let field_ty = field_type_by_name(variant, name);
    let inner = wrap_symbols(variant, rest_symbols, tail);

    match mode {
        FieldMode::Single => {
            if let Some(ty) = field_ty {
                quote_spanned!(name.span() =>
                    __p.each_nonterminal(|#name: #ty, __p| {
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_nonterminal(|#name, __p| {
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
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_opt_nonterminal(|#name, __p| {
                        let #name = #name.unwrap_or_default();
                        #inner
                    })
                )
            }
        }

        FieldMode::Many => {
            if let Some(ty) = field_ty {
                quote_spanned!(name.span() =>
                    __p.each_many_nonterminal(|#name: #ty, __p| {
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_many_nonterminal(|#name, __p| {
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
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_delimited_nonterminal(#open, #optional, #close, |#name, __p| {
                        #inner
                    })
                )
            }
        }

        FieldMode::Comma => {
            if let Some(ty) = field_ty {
                quote_spanned!(name.span() =>
                    __p.each_comma_nonterminal(|#name: #ty, __p| {
                        #inner
                    })
                )
            } else {
                quote_spanned!(name.span() =>
                    __p.each_comma_nonterminal(|#name, __p| {
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
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_nonterminal(|#name, __p| {
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
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
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
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
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
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
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_many_nonterminal(|#name, __p| {
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
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
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_comma_nonterminal(|#name, __p| {
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
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
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name: #ty = Default::default();
                                    #inner
                                }
                            }
                        )
                    } else {
                        quote_spanned!(name.span() =>
                            match __p.expect_keyword(#guard_keyword) {
                                Ok(()) => {
                                    __p.each_delimited_nonterminal(#open, #optional, #close, |#name, __p| {
                                        #inner
                                    })
                                }
                                Err(_) => {
                                    let #name = Default::default();
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

/// Converts a variant name to the keyword string the parser expects.
///
/// Uses snake_case: `FnCall` becomes `"fn_call"`, `Foo` becomes `"foo"`.
fn to_parse_ident(ident: &Ident) -> String {
    ident.to_string().to_case(Case::Snake)
}

/// Returns the identifier to use for a field in generated code.
///
/// Named fields use their declared name; tuple fields get synthetic names `v0`, `v1`, etc.
/// Used both as closure parameter names and in `variant.construct` callbacks.
fn field_ident(field: &syn::Field, index: usize) -> syn::Ident {
    match &field.ident {
        Some(field_name) => field_name.clone(),
        None => syn::Ident::new(&format!("v{}", index), field.span()),
    }
}

/// Like [`field_ident`] but emits `name.clone()` instead of `name`.
///
/// Used in `variant.construct` for code that runs inside `Fn` closures
/// (the `each_*` continuations). Since these closures may be called multiple
/// times (once per ambiguous child parse), the constructor can't move out
/// of captured variables — it must clone.
fn field_ident_cloned(field: &syn::Field, index: usize) -> TokenStream {
    let name = field_ident(field, index);
    quote!(#name.clone())
}

/// Wraps a sequence of struct/tuple bindings in nested `each_nonterminal` calls,
/// separated by commas. Used for `#[cast]` variants and the default `name(a, b)` syntax.
///
/// For example, given bindings `[v0: Wcs, v1: Wcs]` and tail `__p.ok(Prove(v0.clone(), v1.clone()))`:
///
/// ```rust,ignore
/// __p.each_nonterminal(|v0: Wcs, __p| {
///     __p.expect_char(',')?;
///     __p.each_nonterminal(|v1: Wcs, __p| {
///         __p.ok(Prove(v0.clone(), v1.clone()))
///     })
/// })
/// ```
fn wrap_bindings(bindings: &[BindingInfo], tail: TokenStream, current_index: usize) -> TokenStream {
    if bindings.is_empty() {
        return tail;
    }

    let (first, rest) = bindings.split_first().unwrap();
    let name = field_ident(first.ast(), current_index);
    let field_ty = &first.ast().ty;
    let inner = wrap_bindings(rest, tail, current_index + 1);

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
            #inner
        })
    }
}
