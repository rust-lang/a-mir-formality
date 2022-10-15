pub trait AllInto<U> {
    fn all_into(self) -> Vec<U>;
}

impl<T, U> AllInto<U> for Vec<T>
where
    T: Into<U>,
{
    fn all_into(self) -> Vec<U> {
        self.into_iter().map(|t| t.into()).collect()
    }
}

impl<T, U> AllInto<U> for &Vec<T>
where
    T: Into<U> + Clone,
{
    fn all_into(self) -> Vec<U> {
        self.into_iter().map(|t| t.clone().into()).collect()
    }
}
