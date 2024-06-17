extern crate proc_macro;

use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, Attribute};

use crate::{
    attrs,
    spec::{self, FieldMode, FormalitySpec, FormalitySpecSymbol},
};

/// Derive the `Parse` impl, using an optional grammar supplied "from the outside".
/// This is used by the `#[term(G)]` macro, which supplies the grammar `G`.
pub(crate) fn derive_debug_with_spec(
    mut s: synstructure::Structure,
    external_spec: Option<&FormalitySpec>,
) -> TokenStream {
    s.underscore_const(true);

    if let syn::Data::Union(v) = &s.ast().data {
        return syn::Error::new(v.union_token.span, "unions are not supported")
            .into_compile_error();
    }

    let default_debug = default_debug_variant(&s);
    let debug_arms = s.each_variant(|v| debug_variant(v, external_spec));

    s.gen_impl(quote! {
        use ::std::write;

        gen impl std::fmt::Debug for @Self {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
            {
                #[allow(unused_assignments)]
                if fmt.alternate() {
                    #default_debug
                } else {
                    match self {
                        #debug_arms
                    }
                    Ok(())
                }
            }
        }
    })
}

fn default_debug_variant(s: &synstructure::Structure) -> TokenStream {
    let arms = s.each_variant(|v| {
        let fields: TokenStream = v.bindings().iter().map(|bi| {
            if let Some(name) = &bi.ast().ident {
                let name = as_literal(name);
                quote_spanned!(name.span() => .field(#name, #bi))
            } else {
                quote_spanned!(bi.span() => .field(#bi))
            }
        }).collect();
        let variant_name = as_literal(v.ast().ident);
        match v.ast().fields {
            syn::Fields::Named(_) => {
                quote_spanned!(variant_name.span() => fmt.debug_struct(#variant_name) #fields .finish())
            }
            syn::Fields::Unnamed(_) => {
                quote_spanned!(variant_name.span() => fmt.debug_tuple(#variant_name) #fields .finish())
            }
            syn::Fields::Unit => {
                quote_spanned!(variant_name.span() => fmt.debug_tuple(#variant_name) .finish())
            }
        }
    });

    quote_spanned! { s.ast().span() =>
        match self {
            #arms
        }
    }
}

fn debug_variant(
    variant: &synstructure::VariantInfo,
    external_spec: Option<&FormalitySpec>,
) -> TokenStream {
    let ast = variant.ast();

    // When invoked like `#[term(foo)]`, use the spec from `foo`
    if let Some(spec) = external_spec {
        return debug_variant_with_attr(variant, spec);
    }

    // Else, look for a `#[grammar]` attribute on the variant
    if let Some(attr) = get_grammar_attr(ast.attrs) {
        return match attr {
            Ok(spec) => debug_variant_with_attr(variant, &spec),
            Err(err) => err.into_compile_error(),
        };
    }

    // If no `#[grammar(...)]` attribute is provided, then we provide default behavior.

    if variant.bindings().is_empty() {
        // No bindings (e.g., `Foo`) -- just parse a keyword `foo`
        let literal = Literal::string(&to_parse_ident(ast.ident));
        quote! {
            write!(fmt, #literal)?;
        }
    } else if attrs::has_isa_attr(variant.ast().attrs) {
        let streams: Vec<_> = variant
            .bindings()
            .iter()
            .zip(0..)
            .map(|(bi, i)| {
                let binding = &bi.binding;
                if i == 0 {
                    quote! { write!(fmt, "{:?}", #binding)?; }
                } else {
                    quote! { write!(fmt, ", {:?}", #binding)?; }
                }
            })
            .collect();
        quote!(#(#streams)*)
    } else {
        // Otherwise -- parse `variant(binding0, ..., bindingN)`
        let literal = Literal::string(&to_parse_ident(ast.ident));
        let binding_names: Vec<_> = variant.bindings().iter().map(|b| &b.binding).collect();
        quote! {
            fmt.debug_tuple(#literal)
                #(.field(#binding_names))*
                .finish()?;
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
fn debug_variant_with_attr(
    variant: &synstructure::VariantInfo,
    spec: &FormalitySpec,
) -> TokenStream {
    let mut stream = TokenStream::new();

    for (b, index) in variant.bindings().iter().zip(0..) {
        let binding_field = &b.binding;
        let spec_field = field_ident(b.ast(), index);
        stream.extend(quote!(let #spec_field = #binding_field;));
    }

    stream.extend(quote!(let mut sep = "";));

    let mut prev_op: Option<&FormalitySpecSymbol> = None;
    for op in &spec.symbols {
        // insert whitespace if needed
        let suppress_space = match (prev_op, op) {
            // consecutive characters don't need spaces
            (
                Some(spec::FormalitySpecSymbol::Char { .. }),
                spec::FormalitySpecSymbol::Char { .. },
            ) => true,

            // `foo(` looks better than `foo (`
            (
                Some(spec::FormalitySpecSymbol::Keyword { .. }),
                spec::FormalitySpecSymbol::Delimeter { .. },
            ) => true,

            // consecutive delimeters don't need spaces
            (
                Some(spec::FormalitySpecSymbol::Delimeter { .. }),
                spec::FormalitySpecSymbol::Delimeter { .. },
            ) => true,

            _ => false,
        };
        prev_op = Some(op);

        if suppress_space {
            stream.extend(quote!(sep = "";));
        }

        stream.extend(match op {
            spec::FormalitySpecSymbol::Field { name, mode } => debug_field_with_mode(name, mode),

            spec::FormalitySpecSymbol::Keyword { ident } => {
                let literal = as_literal(ident);
                quote_spanned!(ident.span() =>
                    write!(fmt, "{}", sep)?;
                    write!(fmt, "{}", #literal)?;
                    sep = " ";
                )
            }

            spec::FormalitySpecSymbol::Char { punct } => {
                let literal = Literal::character(punct.as_char());
                quote_spanned!(punct.span() =>
                    write!(fmt, "{}", sep)?;
                    write!(fmt, "{}", #literal)?;
                    sep = " ";
                )
            }

            spec::FormalitySpecSymbol::Delimeter { text } => match text {
                '{' | '}' => {
                    let literal = Literal::character(*text);
                    quote!(
                        write!(fmt, "{}", sep)?;
                        write!(fmt, "{}", #literal)?;
                        sep = " ";
                    )
                }
                '(' | '[' => {
                    let literal = Literal::character(*text);
                    quote!(
                        write!(fmt, "{}", #literal)?;
                        sep = "";
                    )
                }
                _ => {
                    let literal = Literal::character(*text);
                    quote!(
                        write!(fmt, "{}", #literal)?;
                        sep = " ";
                    )
                }
            },
        });
    }

    stream
}

fn debug_field_with_mode(name: &Ident, mode: &FieldMode) -> TokenStream {
    match mode {
        FieldMode::Single => {
            quote_spanned! { name.span() =>
                write!(fmt, "{}", sep)?;
                write!(fmt, "{:?}", #name)?;
                sep = " ";
            }
        }

        FieldMode::Optional => {
            quote_spanned! { name.span() =>
                if !::formality_core::util::is_default(#name) {
                    write!(fmt, "{}", sep)?;
                    write!(fmt, "{:?}", #name)?;
                    sep = " ";
                }
            }
        }

        FieldMode::Many => {
            quote_spanned! { name.span() =>
                for e in #name {
                    write!(fmt, "{}", sep)?;
                    write!(fmt, "{:?}", e)?;
                    sep = " ";
                }
            }
        }

        FieldMode::Comma => {
            quote_spanned! { name.span() =>
                for e in #name {
                    write!(fmt, "{}", sep)?;
                    write!(fmt, "{:?}", e)?;
                    sep = ", ";
                }
                sep = " ";
            }
        }

        FieldMode::DelimitedVec {
            open,
            optional,
            close,
        } => {
            let open = Literal::character(*open);
            let close = Literal::character(*close);
            quote_spanned! { name.span() =>
                if !#optional || !#name.is_empty() {
                    write!(fmt, "{}", sep)?;
                    write!(fmt, "{}", #open)?;
                    sep = "";
                    for e in #name {
                        write!(fmt, "{}", sep)?;
                        write!(fmt, "{:?}", e)?;
                        sep = ", ";
                    }
                    write!(fmt, "{}", #close)?;
                    sep = " ";
                }
            }
        }

        FieldMode::Guarded { guard, mode } => {
            let guard = as_literal(guard);
            let base = debug_field_with_mode(name, mode);

            quote_spanned! { name.span() =>
                if !::formality_core::util::is_default(#name) {
                    write!(fmt, "{}{}", sep, #guard)?;
                    sep = " ";
                    #base
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
