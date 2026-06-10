use a_mir_formality::{crates, FormalityTest};

#[test]
fn custom_ptr() {
    FormalityTest::new(crates![crate test {
        struct Ptr {}

        impl Derefable for Ptr {
            type Target = ();
        }

        fn test(ptr: Ptr) -> () {
            let x: () = *ptr;
        }
    }])
    .skip_execute()
    .ok()
}
