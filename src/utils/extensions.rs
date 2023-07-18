use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use std::iter;


pub trait OptionFlatten<'a, T> {
    /// Flattens an `Option<&Option<T>>` to an `Option<&T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(None.flatten(), None);
    /// assert_eq!(Some(&None).flatten(), None);
    /// assert_eq!(Some(&Some(1)).flatten(), Some(&1);
    /// ```
    fn flatten(self) -> Option<&'a T>;
}

impl<'a, T> OptionFlatten<'a, T> for Option<&'a Option<T>> {
    #[inline]
    fn flatten(self) -> Option<&'a T> {
        match self {
            None => None,
            Some(value) => value.as_ref(),
        }
    }
}


pub trait OptionExtensions<T> {
    /// Tests if this option is [`None`] or [`Some(x)`], where `x` passes a predicate.
    #[allow(clippy::wrong_self_convention)]
    #[must_use]
    fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool;
}

impl<T> OptionExtensions<T> for Option<T> {
    #[inline]
    fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            None => true,
            Some(x) => f(x),
        }
    }
}


pub trait ResultExtensions<T, E> {
    /// Changes the error of a result.
    fn ok_or<F>(self, err: F) -> Result<T, F>;
}

impl<T, E> ResultExtensions<T, E> for Result<T, E> {
    #[inline]
    fn ok_or<F>(self, err: F) -> Result<T, F> {
        match self {
            Ok(x) => Ok(x),
            Err(_) => Err(err),
        }
    }
}


pub trait TryCollectOption<T> {
    /// Transforms an iterator of options into an option of collection.
    fn try_collect<B: FromIterator<T>>(self) -> Option<B>;
}

impl<I, T> TryCollectOption<T> for I
where
    I: Iterator<Item=Option<T>>,
{
    #[inline]
    fn try_collect<B: FromIterator<T>>(self) -> Option<B> {
        self.collect()
    }
}


pub trait TryCollectResult<T, E> {
    /// Transforms an iterator of results into a result of collection.
    fn try_collect<B: FromIterator<T>>(self) -> Result<B, E>;
}

impl<I, T, E> TryCollectResult<T, E> for I
where
    I: Iterator<Item=Result<T, E>>,
{
    #[inline]
    fn try_collect<B: FromIterator<T>>(self) -> Result<B, E> {
        self.collect()
    }
}


pub trait VecExtensions<T> {
    /// Creates a new vector of length `n` and fill it with `T::default()`.
    fn with_len(n: usize) -> Self
    where
        T: Default;

    /// Consumes this vector and returns its `n`th element.
    fn into_nth(self, n: usize) -> Option<T>;

    /// If this vector contains a single element, returns it. Otherwise, returns `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(vec![].extract_single_element(), None)
    /// assert_eq!(vec![1].extract_single_element(), Some(1))
    /// assert_eq!(vec![1, 2].extract_single_element(), None)
    /// ```
    fn get_single_element(self) -> Option<T>;

    /// If this vector contains a single element `x`, returns [`Ok(x)`]. Otherwise, returns
    /// [`Err(self)`].
    fn get_single_element_or_self(self) -> Result<T, Self> where Self: Sized;

    /// Puts an element at a specific index in a [`Vec`]. If the vector is not long enough, it is
    /// resized to `index + 1` and unspecified elements are set to `T::default`.
    fn put(&mut self, index: usize, element: T) where T: Default;
}

impl<T> VecExtensions<T> for Vec<T> {
    fn with_len(n: usize) -> Self where T: Default {
        iter::repeat_with(T::default)
            .take(n)
            .collect()
    }

    fn into_nth(self, n: usize) -> Option<T> {
        self.into_iter().nth(n)
    }

    fn get_single_element(mut self) -> Option<T> {
        if self.len() != 1 {
            None
        } else {
            self.pop()
        }
    }

    fn get_single_element_or_self(mut self) -> Result<T, Self> where Self: Sized {
        if self.len() == 1 {
            Ok(self.pop().unwrap())
        } else {
            Err(self)
        }
    }

    fn put(&mut self, index: usize, element: T) where T: Default {
        if self.len() <= index {
            self.resize_with(index + 1, T::default);
        }
        self[index] = element;
    }
}


pub trait MapExtensions<K, V> {
    /// Gets a mutable reference to an element of a map. If the key does not exist in the map, a
    /// default value is created and inserted, and a mutable reference to it is returned.
    fn get_mut_or_create(&mut self, key: K, default: impl FnOnce() -> V) -> &mut V;

    /// Gets a mutable reference to an element of a map. If the key does not exist in the map, the
    /// default value for the type is constructed and inserted, and a mutable reference to it is
    /// returned.
    fn get_mut_or_default(&mut self, key: K) -> &mut V
    where V: Default
    {
        self.get_mut_or_create(key, V::default)
    }

    /// Gets a mutable reference to an element of a map. If the key does not exist in the map, the
    /// default value is created and inserted, and a mutable reference to it is returned.
    fn get_mut_or(&mut self, key: K, default: V) -> &mut V {
        self.get_mut_or_create(key, || default)
    }
}

impl<K: Eq + Hash + Clone, V> MapExtensions<K, V> for HashMap<K, V> {
    fn get_mut_or_create(&mut self, key: K, default: impl FnOnce() -> V) -> &mut V {
        if !self.contains_key(&key) {
            // There is no way that does not invoke using `unsafe` to not clone the key here.
            self.insert(key.clone(), default());
        }
        // SAFETY: If the key was not present, we just inserted it.
        self.get_mut(&key).unwrap()
    }
}

impl<K: Eq + Ord + Clone, V> MapExtensions<K, V> for BTreeMap<K, V> {
    fn get_mut_or_create(&mut self, key: K, default: impl FnOnce() -> V) -> &mut V {
        if !self.contains_key(&key) {
            // There is no way that does not invoke using `unsafe` to not clone the key here.
            self.insert(key.clone(), default());
        }
        // SAFETY: If the key was not present, we just inserted it.
        self.get_mut(&key).unwrap()
    }
}
