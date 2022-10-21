pub trait Matcher<T>: Sized {
    fn try_match(t: &T) -> Option<Self>;
}

impl<A, B> Matcher<Vec<A>> for Vec<B>
where
    B: Matcher<A>,
{
    fn try_match(t: &Vec<A>) -> Option<Self> {
        t.iter().map(|a| B::try_match(a)).collect()
    }
}
