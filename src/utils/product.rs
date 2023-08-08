//! A [`Product`] is a sequence of at least `N` elements, for some `const N: usize`.

use std::array;
use std::iter::Chain;
use std::mem::MaybeUninit;

use crate::utils::extensions::TryCollectResult;

/// A `Product` can be thought of as a vector or a slice with minimal length. All elements are
/// stored on the heap.
#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct Product<T, const MIN_LENGTH: usize>(Box<[T; MIN_LENGTH]>, Vec<T>);

impl<T, const MIN_LENGTH: usize> Product<T, MIN_LENGTH> {
    /// Creates a new [`Product`] with the passed elements.
    pub fn new(fixed_part: [T; MIN_LENGTH], dynamic_part: impl Into<Vec<T>>) -> Self {
        Self(Box::new(fixed_part), dynamic_part.into())
    }

    /// Creates a new [`Product`] with exactly `N` elements.
    pub fn new_minimal(elements: [T; MIN_LENGTH]) -> Self {
        Self::new(elements, Vec::new())
    }

    /// Returns the total number of elements in this product.
    ///
    /// This method always returns a value that is greater than or equal to `MIN_LENGTH`.
    pub fn len(&self) -> usize {
        MIN_LENGTH + self.1.len()
    }

    /// Returns an iterator over references to the elements of this [`Product`].
    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.0.iter().chain(self.1.iter())
    }

    /// Returns an iterator over mutable references to the elements of this [`Product`].
    pub fn iter_mut(&mut self) -> impl Iterator<Item=&mut T> {
        self.0.iter_mut().chain(self.1.iter_mut())
    }

    /// Return a [`Product`] of the same size as this one, with each element being the result of
    /// applying a map to its corresponding one in this [`Product`].
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Product<U, MIN_LENGTH> {
        let mapped_fixed_part = self.0.map(&mut f);
        let mapped_dynamic_part = self.1.into_iter().map(f).collect();
        Product(Box::new(mapped_fixed_part), mapped_dynamic_part)
    }

    /// Returns a [`Product`] of the same size as this one, with each element being the result of
    /// applying a map to a reference to its corresponding one in this [`Product`].
    pub fn map_ref<U>(&self, mut f: impl FnMut(&T) -> U) -> Product<U, MIN_LENGTH> {
        let mapped_fixed_part = array::from_fn(|i| f(&self.0[i]));
        let mapped_dynamic_part = self.1.iter().map(f).collect();
        Product(Box::new(mapped_fixed_part), mapped_dynamic_part)
    }

    /// Tries to apply a map to each element of this [`Product`], in order. If the map returns the
    /// [`Err`] variant for an element, the error is returned immediately. Otherwise, the element is
    /// added to a new [`Product`] of `U`s.
    pub fn try_map<U, E>(self, mut f: impl FnMut(T) -> Result<U, E>) -> Result<Product<U, MIN_LENGTH>, E> {
        let mut mapped_fixed_part = array::from_fn(|_| MaybeUninit::uninit());
        for (i, element) in self.0.into_iter().enumerate() {
            mapped_fixed_part[i] = MaybeUninit::new(f(element)?);
        }
        let mapped_fixed_part = mapped_fixed_part.map(|element| unsafe {
            // SAFETY: We either initialized all the elements of the array in the loop, or we returned
            // early.
            element.assume_init()
        });
        let mapped_dynamic_part = self.1.into_iter().map(f).try_collect()?;
        Ok(Product(Box::new(mapped_fixed_part), mapped_dynamic_part))
    }
}

impl<T, const N: usize> IntoIterator for Product<T, N> {
    type Item = T;
    type IntoIter = Chain<<[T; N] as IntoIterator>::IntoIter, <Vec<T> as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
            .chain(self.1.into_iter())
    }
}

impl<'a, T, const N: usize> IntoIterator for &'a Product<T, N> {
    type Item = &'a T;
    type IntoIter = Chain<<&'a [T; N] as IntoIterator>::IntoIter, <&'a Vec<T> as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
            .chain(self.1.iter())
    }
}

impl<'a, T, const N: usize> IntoIterator for &'a mut Product<T, N> {
    type Item = &'a mut T;
    type IntoIter = Chain<<&'a mut [T; N] as IntoIterator>::IntoIter, <&'a mut Vec<T> as IntoIterator>::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
            .chain(self.1.iter_mut())
    }
}

impl<T, const N: usize> From<Product<T, N>> for Vec<T> {
    fn from(value: Product<T, N>) -> Self {
        value.into_iter().collect()
    }
}


/// A trait for types that may contain a [`Product`], such as the result of collecting an iterator
/// into a [`Product`].
pub trait MaybeProduct: Sized {
    type IntoProduct;

    /// If this is a [`Product`], returns it. Otherwise, returns [`None`].
    fn product(self) -> Option<Self::IntoProduct>;
}


/// Either no element, or a [`Product`] of one or more elements.
pub enum MaybeProduct1<T> {
    None,
    Product(Product<T, 1>),
}

impl<T> FromIterator<T> for MaybeProduct1<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let mut iter = iter.into_iter();
        let Some(first) = iter.next() else {
            return Self::None;
        };
        Self::Product(Product(Box::new([first]), iter.collect()))
    }
}

impl<T, I: IntoIterator<Item=T>> From<I> for MaybeProduct1<T> {
    fn from(value: I) -> Self {
        value.into_iter().collect()
    }
}

impl<T> MaybeProduct for MaybeProduct1<T> {
    type IntoProduct = Product<T, 1>;

    fn product(self) -> Option<Self::IntoProduct> {
        match self {
            Self::Product(product) => Some(product),
            _ => None,
        }
    }
}


/// Either no element, a single element, or a [`Product`] of two or more elements.
pub enum MaybeProduct2<T> {
    None,
    Single(T),
    Product(Product<T, 2>),
}

impl<T> FromIterator<T> for MaybeProduct2<T> {
    fn from_iter<I: IntoIterator<Item=T>>(iter: I) -> Self {
        let mut iter = iter.into_iter();
        let Some(first) = iter.next() else {
            return Self::None;
        };
        let Some(second) = iter.next() else {
            return Self::Single(first);
        };
        Self::Product(Product(Box::new([first, second]), iter.collect()))
    }
}

impl<T, I: IntoIterator<Item=T>> From<I> for MaybeProduct2<T> {
    fn from(value: I) -> Self {
        value.into_iter().collect()
    }
}

impl<T> MaybeProduct for MaybeProduct2<T> {
    type IntoProduct = Product<T, 2>;

    fn product(self) -> Option<Self::IntoProduct> {
        match self {
            Self::Product(product) => Some(product),
            _ => None,
        }
    }
}
