#![expect(dead_code)]
#![expect(unused_variables)]

mod analyze;
mod ast;
mod codegen;
mod parse;
#[cfg(test)]
mod tests;

pub fn judgment(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse::parse(input.into());
    let model = analyze::analyze(ast);
    let tokens = codegen::emit(model);
    proc_macro::TokenStream::from(tokens)
}

mod keyword {
    syn::custom_keyword!(debug);
    syn::custom_keyword!(assert);
    syn::custom_keyword!(trivial);

    syn::custom_keyword!(for_all);
    syn::custom_keyword!(fail);
    syn::custom_keyword!(scope);
    syn::custom_keyword!(with);
}
