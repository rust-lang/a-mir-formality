use rustc_infer::infer::{InferCtxt, NllRegionVariableOrigin, TyCtxtInferExt};
use rustc_middle::mir::{
    self,
    visit::{MutVisitor, TyContext},
    Location,
};
use rustc_middle::ty::{self, SubstsRef, Ty, TyCtxt, TypeFoldable};

/// This function is taken from the compiler.
/// See also https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_borrowck/renumber.rs.html
///
/// Returns the number of created region vars.
pub fn replace_regions_in_mir<'tcx>(tcx: &TyCtxt<'tcx>, body: &mut mir::Body<'tcx>) -> usize {
    tcx.infer_ctxt().enter(|ref infcx| {
        let mut visitor = RenumberVisitor { infcx };
        visitor.visit_body(body);
        infcx.num_region_vars()
    })
}

fn renumber_regions<'tcx, T>(infcx: &InferCtxt<'_, 'tcx>, value: T) -> T
where
    T: TypeFoldable<'tcx>,
{
    infcx.tcx.fold_regions(value, |_region, _depth| {
        let origin = NllRegionVariableOrigin::Existential { from_forall: false };
        infcx.next_nll_region_var(origin)
    })
}

struct RenumberVisitor<'a, 'tcx> {
    infcx: &'a InferCtxt<'a, 'tcx>,
}

impl<'a, 'tcx> RenumberVisitor<'a, 'tcx> {
    fn renumber_regions<T>(&mut self, value: T) -> T
    where
        T: TypeFoldable<'tcx>,
    {
        renumber_regions(self.infcx, value)
    }
}

impl<'a, 'tcx> MutVisitor<'tcx> for RenumberVisitor<'a, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.infcx.tcx
    }

    fn visit_ty(&mut self, ty: &mut Ty<'tcx>, _ty_context: TyContext) {
        *ty = self.renumber_regions(*ty);
    }

    fn visit_substs(&mut self, substs: &mut SubstsRef<'tcx>, _location: Location) {
        *substs = self.renumber_regions(*substs);
    }

    fn visit_region(&mut self, region: &mut ty::Region<'tcx>, _location: Location) {
        let old_region = *region;
        *region = self.renumber_regions(old_region);
    }

    fn visit_const(&mut self, constant: &mut ty::Const<'tcx>, _location: Location) {
        *constant = self.renumber_regions(*constant);
    }
}
