use crate::judgment::ast::Ast;

pub fn analyze(ast: Ast) -> Ast {
    // TODO: Replace panic! with compile_error! and respan
    for rule in ast.rules() {
        if rule.conclusion.name != *ast.name() {
            panic!("Wrong judgment name in conclusion");
        }

        if rule.conclusion.patterns.len() < ast.signature.arguments.len() {
            panic!("fewer patterns in rule than arguments on fn");
        }

        if rule.conclusion.patterns.len() > ast.signature.arguments.len() {
            panic!("more patterns in rule than arguments on fn");
        }
    }

    ast
}
