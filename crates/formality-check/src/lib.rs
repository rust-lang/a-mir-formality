use formality_decl::grammar::{Crate, Program};
use formality_types::grammar::Fallible;

pub fn check_program(program: &Program) -> Fallible<()> {
    Check { program }.check()
}

struct Check<'p> {
    program: &'p Program,
}

impl Check<'_> {
    fn check(&self) -> Fallible<()> {
        let Program { crates } = &self.program;
        for c in crates {
            self.check_crate(c)?;
        }
        Ok(())
    }

    fn check_crate(&self, _c: &Crate) -> Fallible<()> {
        todo!()
    }
}
