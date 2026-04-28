#[test]
fn struct_with_lifetime() {
    crate::assert_ok!(
        [
            crate test {
                struct Foo<'a> {
                    y: &'a u32,
                }

                fn foo() -> () {
                    exists<'y> {
                        let a: u32 = 22 _ u32;
                        let f: Foo<'y> = Foo::<'y> { y: &'y a };
                    }
                }
            }
        ]
    )
}
