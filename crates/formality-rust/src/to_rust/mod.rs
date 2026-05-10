use crate::grammar::{Crates, Fallible, Parameter, ParameterKind, WhereClause, WhereClauseData};
use std::cell::LazyCell;
use std::collections::HashMap;
use std::ops::Deref;

pub mod context;
mod crates;
mod expr;
mod feature_gate;
mod fns;
mod structs_enums_and_adts;
pub mod syntax;
pub mod test_util;
mod traits_and_impls;
mod tys;

pub(crate) const CORE_CRATE_NAME: &'static str = "core";
pub(crate) const MY_CORE_CRATE_NAME: &'static str = "mycore";
pub(crate) const MY_CORE_IMPORT: LazyCell<syntax::ImportItem> =
    LazyCell::new(|| syntax::ImportItem {
        crate_name: MY_CORE_CRATE_NAME.to_string(),
    });

/// Produces a Cargo workspaces on the file system in the
/// `root_direcotry`. After sucesfully creating all the files,
/// `cargo check` is run.
pub fn check_workspace(
    crates: &Crates,
    root_direcotry: &std::path::Path,
) -> Fallible<std::process::Output> {
    let root_toml = create_workspace(crates, root_direcotry)?;

    let output = std::process::Command::new("cargo")
        .env("RUSTFLAGS", "-A warnings")
        .args(["check", "--workspace", "--manifest-path", &root_toml])
        .output()?;
    Ok(output)
}

/// Produces a Cargo workspaces on the file system in the
/// `root_direcotry`. After sucesfully creating all the files,
/// `cargo fmt` is run.
pub fn create_workspace(crates: &Crates, root_directory: &std::path::Path) -> Fallible<String> {
    use std::io::Write;

    let mut ctx = context::Context::default();
    let crates = crates::build_crates(&mut ctx, crates)?;
    let contains_core = crates.get(MY_CORE_CRATE_NAME).is_some();
    let crates_path = root_directory.join("crates");
    let root_toml_path = root_directory.join("Cargo.toml");

    std::fs::create_dir_all(&crates_path)?;

    let root_toml_file = std::fs::File::create(&root_toml_path)?;
    writeln!(&root_toml_file, "[workspace]")?;
    writeln!(&root_toml_file, "resolver = \"2\"")?;
    writeln!(&root_toml_file, "members = [")?;

    for (name, source) in crates {
        let crate_path = crates_path.join(&name);
        let crate_toml_path = crate_path.join("Cargo.toml");
        let src_path = crate_path.join("src");
        let lib_path = src_path.join("lib.rs");
        std::fs::create_dir_all(&src_path)?;

        let mut file = std::fs::File::create(&lib_path)?;
        file.write_all(source.as_bytes())?;

        let file = std::fs::File::create(&crate_toml_path)?;
        writeln!(&file, "[package]")?;
        writeln!(&file, "name = \"{name}\"")?;
        writeln!(&file, "version = \"0.1.0\"")?;
        writeln!(&file, "edition = \"2021\"")?;

        if contains_core && name != MY_CORE_CRATE_NAME {
            writeln!(&file, "[dependencies]")?;
            writeln!(&file, "mycore = {{ path = \"../mycore\"}}")?;
        }

        writeln!(&root_toml_file, "    \"crates/{name}\",")?;
    }

    writeln!(&root_toml_file, "]")?;
    let location = root_toml_path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("Could not convert location to a &str"))?;

    std::process::Command::new("cargo")
        .args(["fmt", "--all", "--manifest-path", location])
        .spawn()?;

    Ok(location.to_string())
}

/// Lowers generic parameters and `where` clauses for a binder.
///
/// Set `skip_first` when lowering generics for a trait
/// declaration.  In trait declarations, the first parameter
/// represents `Self` and must not be treated as a regular generic
/// parameter.
pub fn lower_generics_for_binder(
    ctx: &mut context::Context,
    binder_kinds: &[ParameterKind],
    where_clauses: &[WhereClause],
    names: &[String],
    skip_first: bool,
) -> Fallible<syntax::Generics> {
    if names.len() != binder_kinds.len() {
        anyhow::bail!(
            "binder metadata mismatch: {} names but {} kinds",
            names.len(),
            binder_kinds.len()
        );
    }

    let start = usize::from(skip_first);
    let mut params: HashMap<String, syntax::GenericParam> = names
        .iter()
        .skip(start)
        .cloned()
        .zip(binder_kinds.iter().copied().skip(start))
        .map(|(name, kind)| (name.clone(), generic_param_from_kind(name, kind)))
        .collect();

    for wc in where_clauses {
        match wc {
            WhereClauseData::TypeOfConst(konst, ty) => {
                let name = match tys::lower_const(ctx, konst)? {
                    syntax::ConstExpr::Ident(id) => id,
                    other => anyhow::bail!(
                        "const generic parameter must be an identifier, found `{other}`"
                    ),
                };
                let ty = tys::lower_ty(ctx, ty)?;

                let param = params.get_mut(&name).unwrap();
                match param {
                    syntax::GenericParam::Type(_) => todo!(),
                    syntax::GenericParam::Lifetime(_) => todo!(),
                    syntax::GenericParam::Const {
                        name: _name,
                        ty: existing_ty,
                    } => {
                        *existing_ty = ty;
                    }
                }
            }
            WhereClauseData::IsImplemented(_, _, _) => {
                continue;
            }
            WhereClauseData::AliasEq(_, _) => {
                continue;
            }
            WhereClauseData::Outlives(_, _) => {
                continue;
            }
            WhereClauseData::ForAll(_) => {
                continue;
            }
        }
    }

    let where_clauses = lower_where_clauses(ctx, where_clauses)?;

    Ok(syntax::Generics {
        params,
        where_clauses,
    })
}

pub fn lower_where_clauses(
    ctx: &mut context::Context,
    where_clauses: &[WhereClause],
) -> Fallible<Vec<syntax::WhereClause>> {
    let mut preds = Vec::new();

    for wc in where_clauses {
        let wc = lower_where_clause(ctx, wc)?;
        if let Some(wc) = wc {
            preds.push(wc);
        }
    }

    Ok(preds)
}

fn lower_where_clause(
    ctx: &mut context::Context,
    where_clause: &WhereClause,
) -> Fallible<Option<syntax::WhereClause>> {
    match where_clause {
        WhereClauseData::IsImplemented(ty, trait_id, params) => {
            Ok(Some(syntax::WhereClause::Trait {
                ty: tys::lower_ty(ctx, ty)?,
                trait_name: trait_id.deref().clone(),
                args: params
                    .iter()
                    .map(|param| tys::lower_generic_arg(ctx, param))
                    .collect::<Result<Vec<_>, _>>()?,
            }))
        }
        WhereClauseData::TypeOfConst(_, _) => Ok(None),
        WhereClauseData::AliasEq(_, _) => {
            anyhow::bail!("lowering `AliasEq` where-clauses is not implemented yet")
        }
        WhereClauseData::Outlives(parameter, lifetime) => {
            let lifetime = tys::lower_lt(ctx, lifetime)?;
            let parameter = match parameter {
                Parameter::Ty(ty) => syntax::Parameter::Type(tys::lower_ty(ctx, ty)?),
                Parameter::Lt(lt) => syntax::Parameter::Lifetime(tys::lower_lt(ctx, lt)?),
                Parameter::Const(_) => anyhow::bail!("constant in an outlives constraint"),
            };
            Ok(Some(syntax::WhereClause::Lifetime {
                parameter,
                lifetime,
            }))
        }
        WhereClauseData::ForAll(binder) => {
            let (term, names) = ctx.open_universal(binder);
            Ok(Some(syntax::WhereClause::ForAll(
                lower_generics_for_binder(ctx, binder.kinds(), &[term], &names, false)?,
            )))
        }
    }
}

/// Creates a generic parameter nameed `name` with the given
/// `kind`.
///
/// For const generics, a default type of `usize` is used.  This
/// default should not be relied upon, as it is an implementation
/// detail and my change in the future.
fn generic_param_from_kind(name: String, kind: ParameterKind) -> syntax::GenericParam {
    match kind {
        ParameterKind::Ty => syntax::GenericParam::Type(name),
        ParameterKind::Lt => syntax::GenericParam::Lifetime(name),
        ParameterKind::Const => syntax::GenericParam::Const {
            name,
            ty: syntax::Type::Path {
                name: "###not a type###".to_owned(),
                args: Vec::new(),
            },
        },
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn empty_crate() {
        crate::assert_rust!(
            [
                crate Foo {}
            ],
            ""
        );
    }

    #[test]
    fn simple_feature_gate() {
        crate::assert_rust!(
            [
                crate Foo {
                    #![feature(polonius_alpha)]
                }
            ],
            "#![feature(polonius_alpha)]"
        );
    }

    #[test]
    fn outlives_type() {
        crate::assert_rust!(
            [
                crate core {
                    trait Trait {}
                    struct A<X> where X: Trait {}
                    fn valid<'a, X>(x: &'a A<X>) -> ()
                    where
                        X: 'a,
                        X: Trait,
                    {}
                }
            ],
            r#"
pub trait Trait { }

pub struct A<T1> where T1: Trait {}

pub fn valid<'a2, T3>(mut x: &'a2 A<T3>) -> () where T3: 'a2, T3: Trait {}
"#
        )
    }

    #[test]
    fn outlives_lifetime() {
        crate::assert_rust!(
            [
                crate core {
                    fn foo<'a, 'b>(a: &'a u32) -> &'b u32
                    where 'a: 'b {
                        let r: &'b u32 = identity::<&'b u32>(a);
                        return r;
                    }
                }
            ],
            r#"
pub fn foo<'a0, 'a1>(mut a: &'a0 u32) -> &'a1 u32 where 'a0: 'a1 {
    let mut r: &'a1 u32 = identity::<&'a1 u32>(a);
    return r;
}
"#
        )
    }

    #[test]
    fn forall() {
        crate::assert_rust!(
            [
                crate core {
                    trait A { }

                    trait B where for<'a> &'a u32: A { }
                }
            ],
            r#"
pub trait A { }

pub trait B where for<'a2> &'a2 u32: A { }
        "#
        )
    }
}
