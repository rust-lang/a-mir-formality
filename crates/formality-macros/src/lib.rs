extern crate proc_macro;

mod fold;

synstructure::decl_derive!([Fold] => fold::derive_fold);
