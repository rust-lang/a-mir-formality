
use formality_core::judgment_fn;
use formality_types::grammar::Effect;

judgment_fn! {
    pub fn subset_of(e1: Effect, e2: Effect) => () {
        debug(e1, e2)
        trivial(e1 == e2 => ()) 
        (
            // no premises
            -------- ("runtime")
            (subset_of(_e1, Effect::Runtime) => ())   
        )
        ( 
            ((subset_of(&*e1, &*e2)) => ())   
            -------- ("left-union")
            (subset_of(_e1, Effect::Union(e1, e2)) => ())   
        ) 
        (
            ((subset_of(&*e1, &*e2)) => ())   
            -------- ("right-union")
            (subset_of(_e1, Effect::Union(e1, e2)) => ())   
        )
    }
}
