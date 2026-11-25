use crate::grammar::{
    AdtItem, AtomicPredicate, Predicates, RigidName, RigidTy, Struct, StructBoundData, Ty,
};
use formality_core::judgment_fn;

use crate::prove::{
    decls::Program, prove, prove_after::prove_after, prove_normalize::prove_normalize, Constrained,
};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    /// Built-in rules for `Sized`. Type parameters are only sized through where-clauses, and
    /// `!` and fn types aren't covered yet because programs can't write them.
    pub fn prove_sized(
        _decls: Program,
        env: Env,
        assumptions: Predicates,
        ty: Ty,
    ) => Constraints {
        debug(ty, assumptions, env)

        (
            --- ("scalars")
            (prove_sized(_decls, env, _assumptions, RigidTy { name: RigidName::ScalarId(_), parameters: _ }) => Constraints::none(env))
        )

        (
            // rustc only needs the last element to be sized and checks the rest in WF, but
            // `prove_wf` doesn't, so require all of them here.
            (prove(decls, env, assumptions, Predicates::all_sized(parameters)) => c)
            --- ("tuples")
            (prove_sized(decls, env, assumptions, RigidTy { name: RigidName::Tuple(_), parameters }) => c)
        )

        (
            --- ("references")
            (prove_sized(_decls, env, _assumptions, RigidTy { name: RigidName::Ref(_), parameters: _ }) => Constraints::none(env))
        )

        (
            --- ("raw pointers")
            (prove_sized(_decls, env, _assumptions, RigidTy { name: RigidName::Raw(_), parameters: _ }) => Constraints::none(env))
        )

        (
            // `check_adt` makes sure only the last field can be unsized.
            (if let AdtItem::Struct(Struct { id: _, binder }) = decls.program().adt_item_named(adt_id)?)!
            (let StructBoundData { where_clauses: _, fields } = binder.instantiate_with(parameters)?)
            (prove(decls, env, assumptions, Predicates::all_sized(fields.last().map(|field| &field.ty))) => c)
            --- ("struct")
            (prove_sized(decls, env, assumptions, RigidTy { name: RigidName::AdtId(adt_id), parameters }) => c)
        )

        (
            // `check_adt` makes sure every enum field is sized.
            (if let AdtItem::Enum(_) = decls.program().adt_item_named(adt_id)?)!
            --- ("enum")
            (prove_sized(decls, env, _assumptions, RigidTy { name: RigidName::AdtId(adt_id), parameters: _ }) => Constraints::none(env))
        )

        (
            (prove_normalize(decls, env, assumptions, ty) => Constrained(normalized, c))
            (prove_after(decls, c, assumptions, AtomicPredicate::sized(normalized)) => c)
            --- ("alias")
            (prove_sized(decls, env, assumptions, Ty::AliasTy(_)) => c)
        )
    }
}
