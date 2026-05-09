use crate::grammar::{AdtItem, Crate, CrateItem, Crates, Fallible};
use crate::to_rust::{
    context::Context, feature_gate, fns, structs_enums_and_adts, syntax, traits_and_impls,
};
use std::{collections::HashMap, ops::Deref};

pub fn build_crates(ctx: &mut Context, crates: &Crates) -> Fallible<HashMap<String, String>> {
    // TODO: If core crate:
    // 1. rename it to something like amirformality-core
    // 2. add it as a dependencie to the other crates
    // 3. add import statements in other crates.
    crates
        .crates
        .iter()
        .map(|krate| {
            let lowered = lower_crate(ctx, krate)?;
            Ok((krate.id.deref().clone(), lowered.to_string()))
        })
        .collect()
}

pub fn lower_crate(ctx: &mut Context, krate: &Crate) -> Fallible<syntax::RustCrate> {
    let mut attrs = Vec::new();
    let mut items = Vec::new();

    for item in &krate.items {
        match item {
            CrateItem::FeatureGate(gate) => attrs.push(feature_gate::lower_feature_gate(gate)?),
            CrateItem::AdtItem(AdtItem::Struct(strukt)) => {
                items.push(syntax::Item::Struct(structs_enums_and_adts::lower_struct(
                    ctx, strukt,
                )?));
            }
            CrateItem::AdtItem(AdtItem::Enum(e)) => {
                items.push(syntax::Item::Enum(structs_enums_and_adts::lower_enum(
                    ctx, e,
                )?));
            }
            CrateItem::Trait(t) => {
                items.push(syntax::Item::Trait(traits_and_impls::lower_trait(ctx, t)?));
            }
            CrateItem::TraitImpl(trait_impl) => {
                items.push(syntax::Item::Impl(traits_and_impls::lower_trait_impl(
                    ctx, trait_impl,
                )?));
            }
            CrateItem::NegTraitImpl(neg_trait_impl) => {
                items.push(syntax::Item::NegImpl(
                    traits_and_impls::lower_neg_trait_impl(ctx, neg_trait_impl)?,
                ));
            }
            CrateItem::Fn(function) => {
                items.push(syntax::Item::Function(fns::lower_fn(ctx, function)?));
            }
            CrateItem::Test(_) => {
                todo!("lowering `test` crate items is not implemented yet")
            }
        }
    }

    Ok(syntax::RustCrate { attrs, items })
}
