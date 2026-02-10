use formality_core::term;

#[term]
#[derive(Copy)]
pub enum FeatureGateName {
    #[grammar(polonius_alpha)]
    PoloniusAlpha,
}
