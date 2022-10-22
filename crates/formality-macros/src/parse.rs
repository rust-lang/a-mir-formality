extern crate proc_macro;
use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, Attribute};
use synstructure::BindingInfo;

use crate::spec::{self, FieldMode, FormalitySpec};

/// Derive the `Parse` impl, using an optional grammar supplied "from the outside".
/// This is used by the `#[term(G)]` macro, which supplies the grammar `G`.
pub(crate) fn derive_parse_with_spec(
    s: synstructure::Structure,
    external_spec: Option<&FormalitySpec>,
) -> TokenStream {
    if let syn::Data::Union(v) = &s.ast().data {
        return syn::Error::new(v.union_token.span, "unions are not supported")
            .into_compile_error();
    }

    let mut stream = TokenStream::new();

    if s.variants().len() == 1 {
        stream.extend(parse_variant(&s.variants()[0], external_spec));
    } else {
        stream.extend(quote! {
            let __results = std::iter::empty();
        });
        for variant in s.variants() {
            let variant_name = as_literal(&variant.ast().ident);
            let v = parse_variant(variant, None);
            stream.extend(quote! {
                let __span = tracing::span!(tracing::Level::TRACE, "parse", variant_name = #variant_name);
                let __guard = __span.enter();
                let __results = __results.chain(parse::try_parse(|| { #v }));
                drop(__guard);
            });
        }
        stream.extend(quote! {parse::require_unambiguous(__results)});
    }

    let type_name = as_literal(&s.ast().ident);
    s.gen_impl(quote! {
        use crate::derive_links::{parse};

        gen impl parse::Parse for @Self {
            fn parse<'t>(scope: &parse::Scope, text: &'t str) -> Option<(Self, &'t str)>
            {
                let __span = tracing::span!(tracing::Level::TRACE, "parse", type_name = #type_name, ?scope, ?text);
                let __guard = __span.enter();
                let __result = { #stream };
                tracing::trace!("result = {:?}", __result);
                __result
            }
        }
    })
}

fn parse_variant(
    variant: &synstructure::VariantInfo,
    external_spec: Option<&FormalitySpec>,
) -> TokenStream {
    let ast = variant.ast();

    // When invoked like `#[term(foo)]`, use the spec from `foo`
    if let Some(spec) = external_spec {
        return parse_variant_with_attr(variant, &spec);
    }

    // Else, look for a `#[grammar]` attribute on the variant
    if let Some(attr) = get_grammar_attr(ast.attrs) {
        return match attr {
            Ok(spec) => parse_variant_with_attr(variant, &spec),
            Err(err) => err.into_compile_error(),
        };
    }

    // If no `#[grammar(...)]` attribute is provided, then we provide default behavior.

    if variant.bindings().is_empty() {
        // No bindings (e.g., `Foo`) -- just parse a keyword `foo`
        let literal = Literal::string(&to_parse_ident(&ast.ident));
        let construct = variant.construct(|_, _| quote! {});
        quote! {
            let text = parse::expect_keyword(#literal, text)?;
            Some((#construct, text))
        }
    } else if crate::cast::has_cast_attr(variant.ast().attrs) {
        // Has the `#[cast]` attribute -- just parse the bindings (comma separated, if needed)
        let build: Vec<TokenStream> = parse_bindings(variant.bindings());
        let construct = variant.construct(field_ident);
        quote! {
            #(#build)*
            Some((#construct, text))
        }
    } else {
        // Otherwise -- parse `variant(binding0, ..., bindingN)`
        let literal = Literal::string(&to_parse_ident(&ast.ident));
        let build: Vec<TokenStream> = parse_bindings(variant.bindings());
        let construct = variant.construct(field_ident);
        quote! {
            let text = parse::expect_keyword(#literal, text)?;
            let text = parse::expect_char('(', text)?;
            #(#build)*
            let text = parse::expect_char(',', text).unwrap_or(text); // optional trailing comma
            let text = parse::expect_char(')', text)?;
            Some((#construct, text))
        }
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
) -> TokenStream {
    let mut stream = TokenStream::new();

    for op in &spec.ops {
        stream.extend(match op {
            spec::FormalitySpecOp::Field {
                name,
                mode: FieldMode::Single,
            } => {
                quote_spanned! {
                    name.span() => let (#name, text) = parse::Parse::parse(scope, text)?;
                }
            }

            spec::FormalitySpecOp::Field {
                name,
                mode: FieldMode::Many,
            } => {
                quote_spanned! {
                    name.span() => let (#name, text) = parse::Parse::parse_many(scope, text);
                }
            }

            spec::FormalitySpecOp::Field {
                name,
                mode: FieldMode::Comma,
            } => {
                quote_spanned! {
                    name.span() => let (#name, text) = parse::Parse::parse_comma(scope, text);
                }
            }

            spec::FormalitySpecOp::Keyword { ident } => {
                let literal = as_literal(ident);
                quote_spanned!(ident.span() => let text = parse::expect_keyword(#literal, text)?;)
            }

            spec::FormalitySpecOp::Char { punct } => {
                let literal = Literal::character(punct.as_char());
                quote_spanned!(punct.span() => let text = parse::expect_char(#literal, text)?;)
            }

            spec::FormalitySpecOp::Text { text } => {
                let literal = Literal::string(text);
                quote!(let text = parse::expect_str(#literal, text)?;)
            }
        });
    }

    let c = variant.construct(field_ident);

    stream.extend(quote! {
        Some((#c, text))
    });

    stream
}

fn get_grammar_attr(attrs: &[Attribute]) -> Option<syn::Result<FormalitySpec>> {
    let attr = attrs.iter().find(|a| a.path.is_ident("grammar"))?;
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
                Some(quote!(let text = parse::expect_char(',', text)?;))
            } else {
                None
            };
            quote! {
                #parse_comma
                let (#name, text) = parse::Parse::parse(scope, text)?;
            }
        })
        .collect()
}
