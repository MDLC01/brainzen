pub trait WithOwned: Copy {
    /// The fully owned counterpart.
    type Owned: Copy;

    /// Converts from a type that contains references to [`Copy`] values to a type that just
    /// contains those values.
    fn with_owned(self) -> Self::Owned;
}

impl<T: Copy> WithOwned for &T {
    type Owned = T;

    /// Dereferences a [`Copy`] value.
    ///
    /// This is essentially the base case for all other uses of the [`WithOwned`] trait.
    fn with_owned(self) -> Self::Owned {
        *self
    }
}

impl<T: WithOwned, U: WithOwned> WithOwned for (T, U) {
    type Owned = (T::Owned, U::Owned);

    /// Converts a pair of values that implement [`WithOwned`] to a pair of their owned counterpart.
    ///
    /// In practice, this can be used to convert a pair of references to [`Copy`] values to a pair
    /// of those values.
    ///
    /// # Example
    ///
    /// ```
    /// fn foo(x: (&i32, &i32)) -> (i32, i32) {
    ///     x.with_owned()
    /// }
    /// ```
    #[inline]
    fn with_owned(self) -> Self::Owned {
        let (t, u) = self;
        (t.with_owned(), u.with_owned())
    }
}

impl<T: WithOwned> WithOwned for Option<T> {
    type Owned = Option<T::Owned>;

    /// Converts an [`Option`] of a value that implements [`WithOwned`] to an `Option` of its owned
    /// counterpart.
    ///
    /// In case of an `Option<&T>`, where `T` implements [`Clone`], this is equivalent to
    /// [`Option::<&T>::cloned`].
    ///
    /// # Example
    ///
    /// ```
    /// fn foo(x: Option<(&i32, &i32)>) -> Option<(i32, i32)> {
    ///     x.with_owned()
    /// }
    /// ```
    fn with_owned(self) -> Self::Owned {
        self.map(T::with_owned)
    }
}
