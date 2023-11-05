/// Returns true if `t` is the default value for `t`.
/// Used by the "derive" code for `Debug`.
pub fn is_default<T>(t: &T) -> bool
where
    T: Default + Eq,
{
    let default_value: T = Default::default();
    default_value == *t
}
