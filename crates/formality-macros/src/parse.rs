extern crate proc_macro;

use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, Attribute};
use synstructure::BindingInfo;

use crate::{
    attrs::{has_cast_attr, has_variable_attr, precedence},
    spec::{self, FieldMode, FormalitySpec},
};

/// Derive the `Parse` impl, using an optional grammar supplied "from the outside".
/// This is used by the `#[term(G)]` macro, which supplies the grammar `G`.
pub(crate) fn derive_parse_with_spec(
    s: synstructure::Structure,
    external_spec: Option<&FormalitySpec>,
) -> syn::Result<TokenStream> {
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

    let mut parse_variants = TokenStream::new();
    for variant in s.variants() {
        let variant_name = as_literal(variant.ast().ident);
        let v = parse_variant(variant, external_spec)?;
        let precedence = precedence(&variant.ast().attrs)?.literal();
        parse_variants.extend(quote_spanned!(
            variant.ast().ident.span() =>
            __parser.parse_variant(#variant_name, #precedence, |__p| { #v });
        ));
    }

    let type_name: Literal = as_literal(&s.ast().ident);
    Ok(s.gen_impl(quote! {
        use formality_core::parse;

        gen impl parse::CoreParse<crate::FormalityLang> for @Self {
            fn parse<'t>(scope: &parse::Scope<crate::FormalityLang>, text: &'t str) -> parse::ParseResult<'t, Self>
            {
                let mut __parser = parse::Parser::new(scope, text, #type_name);
                #parse_variants;
                __parser.finish()
            }
        }
    }))
}

fn parse_variant(
    variant: &synstructure::VariantInfo,
    external_spec: Option<&FormalitySpec>,
) -> syn::Result<TokenStream> {
    let ast = variant.ast();

    // When invoked like `#[term(foo)]`, use the spec from `foo`
    if let Some(spec) = external_spec {
        return parse_variant_with_attr(variant, spec);
    }

    // Else, look for a `#[grammar]` attribute on the variant
    if let Some(attr) = get_grammar_attr(ast.attrs) {
        return parse_variant_with_attr(variant, &attr?);
    }

    // If no `#[grammar(...)]` attribute is provided, then we provide default behavior.

    if variant.bindings().is_empty() {
        // No bindings (e.g., `Foo`) -- just parse a keyword `foo`
        let literal = Literal::string(&to_parse_ident(ast.ident));
        let construct = variant.construct(|_, _| quote! {});
        Ok(quote_spanned! {
            ast.ident.span() =>
            __p.expect_keyword(#literal)?;
            Ok(#construct)
        })
    } else if has_variable_attr(variant.ast().attrs) {
        // Has the `#[variable]` attribute -- parse an identifier and then check to see if it is present
        // in the scope. If so, downcast it and check that it has the correct kind.
        Ok(quote_spanned! {
            ast.ident.span() =>
            let v = __p.variable()?;
            Ok(v)
        })
    } else if has_cast_attr(variant.ast().attrs) {
        // Has the `#[cast]` attribute -- just parse the bindings (comma separated, if needed)
        let build: Vec<TokenStream> = parse_bindings(variant.bindings());
        let construct = variant.construct(field_ident);
        Ok(quote_spanned! {
            ast.ident.span() =>
            #(#build)*
            Ok(#construct)
        })
    } else {
        // Otherwise -- parse `variant(binding0, ..., bindingN)`
        let literal = Literal::string(&to_parse_ident(ast.ident));
        let build: Vec<TokenStream> = parse_bindings(variant.bindings());
        let construct = variant.construct(field_ident);
        Ok(quote_spanned! {
            ast.ident.span() =>
            __p.expect_keyword(#literal)?;
            __p.expect_char('(')?;
            #(#build)*
            __p.skip_trailing_comma();
            __p.expect_char(')')?;
            Ok(#construct)
        })
    }
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
) -> syn::Result<TokenStream> {
    let mut stream = TokenStream::new();

    for symbol in &spec.symbols {
        stream.extend(match symbol {
            spec::FormalitySpecSymbol::Field {
                name,
                mode: FieldMode::Single,
            } => {
                quote_spanned! {
                    name.span() =>
                    let #name = __p.nonterminal()?;
                }
            }

            spec::FormalitySpecSymbol::Field {
                name,
                mode: FieldMode::Optional,
            } => {
                quote_spanned! {
                    name.span() =>
                    let #name = __p.opt_nonterminal()?;
                    let #name = #name.unwrap_or_default();
                }
            }

            spec::FormalitySpecSymbol::Field {
                name,
                mode: FieldMode::Many,
            } => {
                quote_spanned! {
                    name.span() =>
                    let #name = __p.many_nonterminal()?;
                }
            }

            spec::FormalitySpecSymbol::Field {
                name,
                mode: FieldMode::Comma,
            } => {
                quote_spanned! {
                    name.span() =>
                    let #name = __p.comma_nonterminal()?;
                }
            }

            spec::FormalitySpecSymbol::Keyword { ident } => {
                let literal = as_literal(ident);
                quote_spanned!(ident.span() =>
                    let () = __p.expect_keyword(#literal)?;
                )
            }

            spec::FormalitySpecSymbol::Char { punct } => {
                let literal = Literal::character(punct.as_char());
                quote_spanned!(
                    punct.span() =>
                    let () = __p.expect_char(#literal)?;
                )
            }

            spec::FormalitySpecSymbol::Delimeter { text } => {
                let literal = Literal::character(*text);
                quote!(
                    let () = __p.expect_char(#literal)?;
                )
            }
        });
    }

    let c = variant.construct(field_ident);

    stream.extend(quote! {
        Ok(#c)
    });

    Ok(stream)
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

/// Creates code to parse `b0, b1, ..., bN` where `bi` is a binding
fn parse_bindings(bindings: &[BindingInfo]) -> Vec<TokenStream> {
    bindings
        .iter()
        .zip(0..)
        .map(|(b, index)| {
            let name = field_ident(b.ast(), index);
            let parse_comma = if index > 0 {
                Some(quote_spanned!(
                    name.span() =>
                    __p.expect_char(',')?;
                ))
            } else {
                None
            };
            quote_spanned! {
                name.span() =>
                #parse_comma
                let #name = __p.nonterminal()?;
            }
        })
        .collect()
}
