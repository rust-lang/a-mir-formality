extern crate proc_macro;

mod fold;
mod parse;

synstructure::decl_derive!([Fold] => fold::derive_fold);
synstructure::decl_derive!([Parse, attributes(formality)] => parse::derive_parse);
