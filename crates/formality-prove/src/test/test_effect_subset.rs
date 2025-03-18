#[test]
fn test_some_atomic_effect() {
    let goal: Wc = term("Debug(Vec<u32>)");
    prove(decls(), (), (), goal).assert_ok(expect![[]]);
}