use rustc_hir::def_id::DefId;
use rustc_middle::ty::TyCtxt;
use rustc_span::Symbol;

mod decl;
mod mir;
mod ty;

pub struct FormalityGen<'tcx> {
    pub tcx: TyCtxt<'tcx>,
}

impl<'tcx> FormalityGen<'tcx> {
    // since the tick character ' is reserved in Racket, we use % instead
    const LIFETIME_MARKER: &'static str = "%";

    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        FormalityGen { tcx }
    }

    pub fn generate(&self) -> String {
        let program = self.emit_program();
        format!(
            r#"
#lang racket
(require redex/reduction-semantics
         "../../racket-src/ty/user-ty.rkt"
         "../../racket-src/rust/grammar.rkt"
         "../../racket-src/rust/libcore.rkt"
         "../../racket-src/rust/prove.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [(Rust/Program (term {program}))
    ]

   (test-equal #t (term (rust:is-program-ok Rust/Program)))
   )
  )"#
        )
    }

    pub fn emit_ident(&self, ident: &Symbol) -> String {
        // Racket special characters: ( ) [ ] { } " , ' ` ; # | \
        ident.as_str().replace('\'', Self::LIFETIME_MARKER)
    }

    pub fn emit_def_path(&self, def_id: DefId) -> String {
        let def_path = self.tcx.def_path_str(def_id);
        match def_path.as_str() {
            "std::marker::Sized" => "core:Sized".to_string(),
            "std::marker::Copy" => "core:Copy".to_string(),
            _ => def_path,
        }
    }
}
