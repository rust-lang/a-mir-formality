#![cfg(test)]

use std::cell::Cell;

use crate::judgment_fn;

thread_local! {
    static EXECUTIONS: Cell<usize> = const { Cell::new(0) };
}

judgment_fn! {
    fn proof_metadata_cycle() => u32 {
        debug()

        (
            (let () = EXECUTIONS.set(EXECUTIONS.get() + 1))
            --- ("base")
            (proof_metadata_cycle() => 0)
        )

        (
            (proof_metadata_cycle() => value)
            --- ("recursive")
            (proof_metadata_cycle() => value)
        )
    }
}

#[test]
fn cyclic_judgment_converges_by_value_and_retains_its_base_proof() {
    EXECUTIONS.set(0);

    let (value, proof) = proof_metadata_cycle().into_singleton().unwrap();

    assert_eq!(value, 0);
    assert_eq!(EXECUTIONS.get(), 2);
    assert_eq!(proof.rule_name, Some("base"));
}
