use formality_decl::grammar::Adt;
use formality_types::grammar::Fallible;

impl super::Check<'_> {
    pub(super) fn check_adt(&self, adt: &Adt) -> Fallible<()> {
        let Adt { kind, id, binder } = adt;


        
        Ok(())
    }
}
