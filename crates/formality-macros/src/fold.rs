extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use quote::ToTokens;
use syn::{parse_quote, DeriveInput, Ident, TypeParam, TypeParamBound};

use synstructure::decl_derive;

fn derive_fold(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Ref);

    let substitute_body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        vi.construct(|_, index| {
            let bind = &bindings[index];
            quote! {
                ::formality_types::fold::Fold::substitute(#bind, substitution_fn)
            }
        })
    });

    let free_variables_body = s
        .each(|field| quote!(output.extend(::formality_types::fold::Fold::free_variables(#field))));

    let shift_in_body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        vi.construct(|_, index| {
            let bind = &bindings[index];
            quote! {
                ::formality_types::fold::Fold::shift_in(#bind)
            }
        })
    });

    s.add_bounds(synstructure::AddBounds::None);
    s.bound_impl(
        quote!(::formality_types::fold::Fold<#interner>),
        quote! {
            fn try_fold_with<E>(
                self,
                folder: &mut dyn ::chalk_ir::fold::FallibleTypeFolder < #interner, Error = E >,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::std::result::Result<Self, E> {
                Ok(match self { #body })
            }
        },
    )
}

fn derive_fallible_type_folder(mut s: synstructure::Structure) -> TokenStream {
    let interner = try_find_interner(&mut s).map_or_else(
        || {
            s.add_impl_generic(parse_quote! { _I });
            s.add_where_predicate(parse_quote! { _I: ::chalk_ir::interner::Interner });
            quote! { _I }
        },
        |(interner, _)| interner,
    );
    s.underscore_const(true);
    s.unbound_impl(
        quote!(::chalk_ir::fold::FallibleTypeFolder<#interner>),
        quote! {
            type Error = ::core::convert::Infallible;

            fn as_dyn(&mut self) -> &mut dyn ::chalk_ir::fold::FallibleTypeFolder<I, Error = Self::Error> {
                self
            }

            fn try_fold_ty(
                &mut self,
                ty: ::chalk_ir::Ty<#interner>,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Ty<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_ty(self, ty, outer_binder))
            }

            fn try_fold_lifetime(
                &mut self,
                lifetime: ::chalk_ir::Lifetime<#interner>,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Lifetime<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_lifetime(self, lifetime, outer_binder))
            }

            fn try_fold_const(
                &mut self,
                constant: ::chalk_ir::Const<#interner>,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Const<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_const(self, constant, outer_binder))
            }

            fn try_fold_program_clause(
                &mut self,
                clause: ::chalk_ir::ProgramClause<#interner>,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::ProgramClause<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_program_clause(self, clause, outer_binder))
            }

            fn try_fold_goal(
                &mut self,
                goal: ::chalk_ir::Goal<#interner>,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Goal<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_goal(self, goal, outer_binder))
            }

            fn forbid_free_vars(&self) -> bool {
                ::chalk_ir::fold::TypeFolder::forbid_free_vars(self)
            }

            fn try_fold_free_var_ty(
                &mut self,
                bound_var: ::chalk_ir::BoundVar,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Ty<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_free_var_ty(self, bound_var, outer_binder))
            }

            fn try_fold_free_var_lifetime(
                &mut self,
                bound_var: ::chalk_ir::BoundVar,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Lifetime<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_free_var_lifetime(self, bound_var, outer_binder))
            }

            fn try_fold_free_var_const(
                &mut self,
                ty: ::chalk_ir::Ty<#interner>,
                bound_var: ::chalk_ir::BoundVar,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Const<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_free_var_const(self, ty, bound_var, outer_binder))
            }

            fn forbid_free_placeholders(&self) -> bool {
                ::chalk_ir::fold::TypeFolder::forbid_free_placeholders(self)
            }

            fn try_fold_free_placeholder_ty(
                &mut self,
                universe: ::chalk_ir::PlaceholderIndex,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Ty<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_free_placeholder_ty(self, universe, outer_binder))
            }

            fn try_fold_free_placeholder_lifetime(
                &mut self,
                universe: ::chalk_ir::PlaceholderIndex,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Lifetime<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_free_placeholder_lifetime(self, universe, outer_binder))
            }

            fn try_fold_free_placeholder_const(
                &mut self,
                ty: ::chalk_ir::Ty<#interner>,
                universe: ::chalk_ir::PlaceholderIndex,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Const<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_free_placeholder_const(self, ty, universe, outer_binder))
            }

            fn forbid_inference_vars(&self) -> bool {
                ::chalk_ir::fold::TypeFolder::forbid_inference_vars(self)
            }

            fn try_fold_inference_ty(
                &mut self,
                var: ::chalk_ir::InferenceVar,
                kind: ::chalk_ir::TyVariableKind,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Ty<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_inference_ty(self, var, kind, outer_binder))
            }

            fn try_fold_inference_lifetime(
                &mut self,
                var: ::chalk_ir::InferenceVar,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Lifetime<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_inference_lifetime(self, var, outer_binder))
            }

            fn try_fold_inference_const(
                &mut self,
                ty: ::chalk_ir::Ty<#interner>,
                var: ::chalk_ir::InferenceVar,
                outer_binder: ::chalk_ir::DebruijnIndex,
            ) -> ::core::result::Result<::chalk_ir::Const<#interner>, Self::Error> {
                ::core::result::Result::Ok(::chalk_ir::fold::TypeFolder::fold_inference_const(self, ty, var, outer_binder))
            }

            fn interner(&self) -> #interner {
                ::chalk_ir::fold::TypeFolder::interner(self)
            }
        },
    )
}
