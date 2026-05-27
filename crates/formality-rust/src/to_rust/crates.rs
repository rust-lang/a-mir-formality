use crate::grammar::{AdtItem, Crate, CrateItem, Crates, Fallible};
use crate::to_rust::{
    context::Context, feature_gate, fns, structs_enums_and_adts, syntax, traits_and_impls,
    CORE_CRATE_NAME, MY_CORE_CRATE_NAME, MY_CORE_IMPORT,
};
use std::collections::HashMap;

pub fn build_crates(ctx: &mut Context, crates: &Crates) -> Fallible<HashMap<String, String>> {
    let contains_core = crates
        .crates
        .iter()
        .find(|c| *(c.id) == CORE_CRATE_NAME)
        .is_some();

    crates
        .crates
        .iter()
        .map(|krate| {
            let lowered = lower_crate(ctx, krate, contains_core)?;
            let name = if *(krate.id) == CORE_CRATE_NAME {
                MY_CORE_CRATE_NAME.to_string()
            } else {
                (*(krate.id)).clone()
            };
            Ok((name, lowered.to_string()))
        })
        .collect()
}

pub fn lower_crate(
    ctx: &mut Context,
    krate: &Crate,
    add_core_import: bool,
) -> Fallible<syntax::RustCrate> {
    let mut attrs = Vec::new();
    // Do not import the core crate in itself.
    let mut items = if add_core_import && *(krate.id) != CORE_CRATE_NAME {
        vec![syntax::Item::Import((*MY_CORE_IMPORT).clone())]
    } else {
        Vec::new()
    };

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
                items.push(syntax::Item::Function(fns::lower_fn(
                    ctx,
                    function,
                    syntax::Visibility::Public,
                )?));
            }
            CrateItem::Test(_) => {
                todo!("lowering `test` crate items is not implemented yet")
            }
        }
    }

    Ok(syntax::RustCrate { attrs, items })
}
