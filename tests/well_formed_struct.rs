use a_mir_formality::{crates, FormalityTest};

#[test]
fn struct_with_lifetime() {
    FormalityTest::new(crates![crate test {
        struct Foo<'a> {
            y: &'a u32,
        }

        fn main() -> () {
            exists<'y> {
                let a: u32 = 22_u32;
                let f: Foo<'y> = Foo::<'y> { y: &'y a };
            }
        }
    }])
    .ok()
}
