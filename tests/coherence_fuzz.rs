use bolero;
use formality_rust::grammar::Program;

fn main() {
    bolero::check!().with_type::<Program>().for_each(|p| {});
}
