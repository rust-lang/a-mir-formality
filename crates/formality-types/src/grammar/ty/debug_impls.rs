use super::{AliasName, AliasTy, AssociatedTyName, Parameter, RefKind, RigidName, RigidTy};
use std::fmt::Debug;

// ANCHOR: RigidTy_impl
impl Debug for RigidTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let RigidTy { name, parameters } = self;
        match name {
            RigidName::AdtId(name) => {
                write!(
                    f,
                    "{:?}{:?}",
                    name,
                    PrettyParameters::new("<", ">", parameters)
                )
            }
            RigidName::ScalarId(s) if parameters.is_empty() => {
                write!(f, "{:?}", s)
            }
            RigidName::Ref(RefKind::Shared) if parameters.len() == 2 => {
                write!(f, "&{:?} {:?}", parameters[0], parameters[1])
            }
            RigidName::Ref(RefKind::Mut) if parameters.len() == 2 => {
                write!(f, "&mut {:?} {:?}", parameters[0], parameters[1])
            }
            RigidName::Tuple(arity) if parameters.len() == *arity => {
                if *arity != 0 {
                    write!(f, "{:?}", PrettyParameters::new("(", ")", parameters))
                } else {
                    // PrettyParameters would skip the separators
                    // for 0 arity
                    write!(f, "()")
                }
            }
            RigidName::FnDef(name) => {
                let parameters = PrettyParameters::new("<", ">", parameters);
                write!(f, "fn {name:?}{parameters:?}",)
            }
            RigidName::FnPtr(arity) if parameters.len() == *arity + 1 => {
                let len = parameters.len();
                if *arity != 0 {
                    write!(
                        f,
                        "fn{:?} -> {:?}",
                        PrettyParameters::new("(", ")", &parameters[..len - 1]),
                        parameters[len - 1]
                    )
                } else {
                    // PrettyParameters would skip the separators
                    // for 0 arity
                    write!(f, "fn() -> {:?}", parameters[len - 1])
                }
            }
            _ => {
                write!(f, "{:?}{:?}", name, PrettyParameters::angle(parameters))
            }
        }
    }
}
// ANCHOR_END: RigidTy_impl

impl Debug for AliasTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let AliasTy { name, parameters } = self;
        match name {
            AliasName::AssociatedTyId(AssociatedTyName {
                trait_id,
                item_id,
                item_arity,
            }) => {
                let (trait_parameters, item_parameters) =
                    parameters.split_at(parameters.len() - item_arity);
                let (self_parameter, other_parameters) = trait_parameters.split_at(1);
                // Grr, wish we would remember the number of parameters assigned to each position.
                write!(
                    f,
                    "<{:?} as {:?}{:?}>::{:?}{:?}",
                    self_parameter[0],
                    trait_id,
                    PrettyParameters::angle(other_parameters),
                    item_id,
                    PrettyParameters::angle(item_parameters),
                )
            }
        }
    }
}

struct PrettyParameters<'a> {
    open: &'a str,
    close: &'a str,
    p: &'a [Parameter],
}
impl<'a> PrettyParameters<'a> {
    fn new(open: &'a str, close: &'a str, p: &'a [Parameter]) -> Self {
        Self { open, close, p }
    }

    fn angle(p: &'a [Parameter]) -> Self {
        Self::new("<", ">", p)
    }
}

impl Debug for PrettyParameters<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.p.is_empty() {
            Ok(())
        } else {
            write!(f, "{}", self.open)?;
            write!(f, "{:?}", self.p[0])?;
            for p in &self.p[1..] {
                write!(f, ", {:?}", p)?;
            }
            write!(f, "{}", self.close)?;
            Ok(())
        }
    }
}
