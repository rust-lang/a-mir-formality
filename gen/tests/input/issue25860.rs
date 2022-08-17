static UNIT: &'static &'static () = &&();

fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T {
    v
}

fn bad<'a, T>(x: &'a T) -> &'static T {
    let f: fn(_, &'a T) -> &'static T = foo;
    f(UNIT, x)
}

fn main() {
    let value = {
        let data = 22;
        bad(&data)
    };
    let _ = *value;
}
