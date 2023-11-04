//! Functions to manipulate the custom attributes that guide our macros in various ways.

use syn::{spanned::Spanned, Attribute, DeriveInput};

use crate::{custom::Customize, precedence::Precedence};

/// Checks for any kind of attribute that indicates an "is-a" relationship,
/// e.g. `#[cast]` and `#[variable]`.
///
/// See [`has_cast_attr`][] and [`has_variable_attr`][] for more details.
pub(crate) fn has_isa_attr(attrs: &[Attribute]) -> bool {
    has_cast_attr(attrs) || has_variable_attr(attrs)
}

/// The `#[cast]` attribute is placed on enum variants with a single binding,
/// like `enum Foo { #[cast] Bar(Bar), ... }`. It indicates that `Foo` is an
/// extension of `Bar`, so we should permit upcasts from `Bar` to `Foo`
/// and generally ignore the fact that `Bar` is a variant of `Foo` (for example,
/// in debug print-outs, we don't print `Bar(...)`, we just print `...`).
pub(crate) fn has_cast_attr(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|a| a.path().is_ident("cast"))
}

/// The `#[variable]` attribute is a special-case of `#[cast]` that is used
/// for variables. It can only be used on a single-entry variant in some type
/// that is up/down-castable to the language's `Parameter` type (e.g., in Rust,
/// this could be used in `Ty`, `Lifetime`, or `Const`).
///
/// `#[variable]` has subtle effects on folding and parsing:
///
/// * When folding a variable variant, we apply the substitution function, which yields
///   a Parameter. We then downcast that to the type needed. If that downcast
///   fails, we panic, as that indicates an ill-kinded substitution.
/// * When parsing a variable variant, we parse an identifier and then check the in-scope
///   bindings for variables with that name. If any are found, we extract the result
///   (a parameter) and downcast it. If the resulting downcast fails, that is considered
///   a parse error (ill-kinded term).
pub(crate) fn has_variable_attr(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|a| a.path().is_ident("variable"))
}

/// Extract a `#[precedence]` level, defaults to 0
pub(crate) fn precedence(attrs: &[Attribute]) -> syn::Result<Precedence> {
    parse_attr_named(attrs, "precedence")
}

/// Extracts any customization attribute from a list of attributes.
pub(crate) fn customize(attrs: &[Attribute]) -> syn::Result<Customize> {
    parse_attr_named(attrs, "customize")
}

fn parse_attr_named<T>(attrs: &[Attribute], name: &str) -> syn::Result<T>
where
    T: Default + syn::parse::Parse,
{
    let mut v: Vec<T> = attrs
        .iter()
        .filter(|a| a.path().is_ident(name))
        .map(|a| a.parse_args())
        .collect::<syn::Result<_>>()?;

    if v.len() > 1 {
        Err(syn::Error::new(
            attrs
                .iter()
                .filter(|a| a.path().is_ident(name))
                .skip(1)
                .next()
                .unwrap()
                .path()
                .span(),
            format!("multiple `{}` attributes", name),
        ))
    } else if v.len() == 1 {
        Ok(v.pop().unwrap())
    } else {
        Ok(T::default())
    }
}

/// Removes all attributes from the input that are specific to formality.
pub(crate) fn remove_formality_attributes(input: &mut DeriveInput) {
    remove_formality_attributes_from_vec(&mut input.attrs);
    if let syn::Data::Enum(v) = &mut input.data {
        for variant in &mut v.variants {
            remove_formality_attributes_from_vec(&mut variant.attrs);
        }
    }
}

fn remove_formality_attributes_from_vec(attrs: &mut Vec<Attribute>) {
    attrs.retain(|attr| {
        !attr.path().is_ident("grammar")
            && !attr.path().is_ident("cast")
            && !attr.path().is_ident("variable")
            && !attr.path().is_ident("customize")
            && !attr.path().is_ident("precedence")
    });
}
