//! Functions to manipulate the custom attributes that guide our macros in various ways.

use syn::{Attribute, DeriveInput};

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
    });
}
