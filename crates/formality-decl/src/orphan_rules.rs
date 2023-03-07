// use contracts::requires;
// use formality_types::{
//     cast::Upcast,
//     grammar::{
//         AliasTy, EnsuresTy, Fallible, ImplicationTy, Parameter, ParameterData, PredicateTy,
//         RigidName, RigidTy, TraitRef, TyData,
//     },
// };

// use crate::grammar::Program;

// impl Program {
//     pub fn orphan_check(&self, env: &Env, trait_ref: &TraitRef) -> Fallible<bool> {
//         // From https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html:
//         //
//         // Given `impl<P1..=Pn> Trait<T1..=Tn> for T0`, an impl is valid only if at least one of the following is true:
//         //
//         // - `Trait` is a local trait
//         // - All of
//         //  - At least one of the types `T0..=Tn` must be a local type. Let `Ti` be the
//         //    first such type.
//         //  - No uncovered type parameters `P1..=Pn` may appear in `T0..Ti` (excluding
//         //    `Ti`)
//         //
//         // Given the following definitions:
//         //
//         // Covered Type: A type which appears as a parameter to another type. For example,
//         // `T` is uncovered, but the `T` in `Vec<T>` is covered. This is only relevant for
//         // type parameters.
//         //
//         // Fundamental Type: A type for which you cannot add a blanket impl backwards
//         // compatibly. This includes `&`, `&mut`, and `Box`. Any time a type `T` is
//         // considered local, `&T`, `&mut T`, and `Box<T>` are also considered local.
//         // Fundamental types cannot cover other types. Any time the term "covered type" is
//         // used, `&T`, `&mut T`, and `Box<T>` are not considered covered.
//         //
//         // Local Type: A struct, enum, or union which was defined in the current crate.
//         // This is not affected by type parameters. `struct Foo` is considered local, but
//         // `Vec<Foo>` is not. `LocalType<ForeignType>` is local. Type aliases and trait
//         // aliases do not affect locality.

//         Ok(self.trait_is_local(env, trait_ref)? || self.type_is_local(env, trait_ref)?)
//     }

//     fn trait_is_local(&self, _env: &Env, trait_ref: &TraitRef) -> Fallible<bool> {
//         // "`Trait` is a local trait"
//         let current_crate_id = self.current_crate_id();
//         let crate_id = self.crate_defining_trait_named(&trait_ref.trait_id)?;
//         Ok(current_crate_id == crate_id)
//     }

//     fn type_is_local(&self, env: &Env, trait_ref: &TraitRef) -> Fallible<bool> {
//         let universe = env.term_universe(trait_ref);

//         // "`Ti` is a local type and no uncovered type parameters appear in `T0..Ti`"
//         for i in 0..trait_ref.parameters.len() {
//             if self.is_local_parameter(env, &trait_ref.parameters[i])? {
//                 if !self.have_uncovered_placeholder_visible_from(
//                     env,
//                     &trait_ref.parameters[..i],
//                     universe,
//                 )? {
//                     return Ok(true);
//                 }
//             }
//         }

//         // Anything else fails.
//         Ok(false)
//     }

//     #[requires(env.fully_refreshed(parameter))]
//     fn is_local_parameter(&self, env: &Env, parameter: &Parameter) -> Fallible<bool> {
//         match parameter.data() {
//             ParameterData::Ty(ty) => match ty {
//                 TyData::RigidTy(RigidTy {
//                     name,
//                     parameters: _,
//                 }) => match name {
//                     // User-defined types are local if declared in the current crate
//                     RigidName::AdtId(adt_id) => {
//                         // FIXME: fundamental
//                         Ok(self.current_crate_id() == self.crate_defining_adt_named(adt_id)?)
//                     }

//                     // Scalars and built-in types are local to core.
//                     RigidName::ScalarId(_) | RigidName::Ref(_) | RigidName::FnPtr(_) => {
//                         Ok(self.is_core_crate())
//                     }

//                     RigidName::Tuple(_) => todo!(),

//                     RigidName::FnDef(_) => todo!(),
//                 },

//                 // Alias types (e.g., associated types) are not considered local; they can be normalized to anything.
//                 TyData::AliasTy(_) => Ok(false),

//                 TyData::PredicateTy(p) => match p {
//                     PredicateTy::ForAll(binder) | PredicateTy::Exists(binder) => {
//                         let mut env = env.clone();
//                         let ty = env.instantiate_universally(binder).upcast();
//                         self.is_local_parameter(&env, &ty)
//                     }
//                     PredicateTy::ImplicationTy(ImplicationTy { predicates: _, ty })
//                     | PredicateTy::EnsuresTy(EnsuresTy { predicates: _, ty }) => {
//                         self.is_local_parameter(&env, &ty.upcast())
//                     }
//                 },

//                 //
//                 TyData::Variable(_) => Ok(false),
//             },

//             // Lifetimes are not local.
//             ParameterData::Lt(_) => Ok(false),
//         }
//     }

//     #[requires(parameters.iter().all(|p| env.fully_refreshed(p)))]
//     fn have_uncovered_placeholder_visible_from(
//         &self,
//         env: &Env,
//         parameters: &[Parameter],
//         universe: Universe,
//     ) -> Fallible<bool> {
//         for p in parameters {
//             if self.has_uncovered_placeholder_visible_from(env, p, universe)? {
//                 return Ok(true);
//             }
//         }
//         Ok(false)
//     }

//     #[requires(env.fully_refreshed(parameter))]
//     fn has_uncovered_placeholder_visible_from(
//         &self,
//         env: &Env,
//         parameter: &Parameter,
//         universe: Universe,
//     ) -> Fallible<bool> {
//         match parameter.data() {
//             ParameterData::Ty(ty) => match ty {
//                 TyData::RigidTy(RigidTy { name, parameters }) => {
//                     if self.is_fundamental(name) {
//                         self.have_uncovered_placeholder_visible_from(env, parameters, universe)
//                     } else {
//                         Ok(false)
//                     }
//                 }

//                 TyData::PredicateTy(p) => match p {
//                     PredicateTy::ForAll(binder) | PredicateTy::Exists(binder) => {
//                         let mut env = env.clone();
//                         let ty = env.instantiate_universally(binder).upcast();
//                         self.has_uncovered_placeholder_visible_from(&env, &ty, universe)
//                     }
//                     PredicateTy::ImplicationTy(ImplicationTy { predicates: _, ty })
//                     | PredicateTy::EnsuresTy(EnsuresTy { predicates: _, ty }) => {
//                         self.has_uncovered_placeholder_visible_from(&env, &ty.upcast(), universe)
//                     }
//                 },

//                 TyData::AliasTy(AliasTy {
//                     name: _,
//                     parameters,
//                 }) => self.have_uncovered_placeholder_visible_from(env, parameters, universe),

//                 // Base case:
//                 TyData::Variable(v) => {
//                     if env.universe(*v) <= universe {
//                         Ok(true)
//                     } else {
//                         Ok(false)
//                     }
//                 }
//             },

//             // Lifetimes are not considered uncovered placeholders.
//             ParameterData::Lt(_) => Ok(false),
//         }
//     }

//     fn is_fundamental(&self, name: &RigidName) -> bool {
//         match name {
//             RigidName::AdtId(_) => {
//                 // FIXME
//                 false
//             }

//             RigidName::FnDef(_) => false,

//             RigidName::ScalarId(_) => false,

//             RigidName::Ref(_) => true,

//             RigidName::Tuple(_) => todo!(),

//             RigidName::FnPtr(_) => todo!(),
//         }
//     }
// }
