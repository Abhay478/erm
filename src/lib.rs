//! Exports one type, `Erm`, which is an enum: "Yes, No, Maybe, Maybe Not, and Don't Know".
//! 
//! Inspired by the frequent usage of the word "erm" in Harry Potter. Much more flavourful than "umm", if you ask me.
//! 
//! If the whimsical nature of this library is not to your taste, I apologise. 
//! 
//! Also, no std. Nice.

use core::panic;
use core::{hint, iter::{FusedIterator}, ops::{Deref, DerefMut}};

/// You can use this like an `Option` with the first two variants, but you can do...more.
/// 
/// 
#[derive(Clone, PartialEq, Copy, Eq, Debug, Hash)]
pub enum Erm<T> {
    /// This is Option::Some. It works.
    Yes(T),
    /// This is Option::None. It does not work.
    No,
    /// Outputs of randomised procedures, for example, can be represented with this variant.
    /// It means you're confident enough in the outcome that you'd like to go ahead with it,
    /// while making sure you remember that stuff could go wrong.
    Maybe(T),
    /// This variant is for when it's probably a good idea to treat the outcome as a failure,
    /// but if you're really optimistic, you can still try it.
    MaybeNot,
    /// No promises. You really should retry or something.
    DontKnow,
}

impl <T> Erm<T> {

    /// Like is_some().
    pub fn is(&self) -> bool {
        matches!(self, Erm::Yes(_))
    }

    /// Like is_some_and().
    pub fn is_and(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            Erm::Yes(x) => f(x),
            _ => false,
        }
    }

    /// Like is_none().
    pub fn is_not(&self) -> bool {
        matches!(self, Erm::No)
    }

    /// Like is_none_or().
    pub fn is_not_or(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            Erm::No => true,
            Erm::Yes(x) => f(x),
            _ => false,
        }
    }

    /// Optimistic.
    pub fn might_be(&self) -> bool {
        matches!(self, Erm::Maybe(_) | Erm::Yes(_))
    }

    /// Pessimistic.
    pub fn might_not_be(&self) -> bool {
        matches!(self, Erm::MaybeNot | Erm::No)
    }

    pub fn known(&self) -> bool {
        !matches!(self, Erm::DontKnow)
    }

    pub fn unknown(&self) -> bool {
        matches!(self, Erm::DontKnow)
    }

    

    pub const fn as_ref(&self) -> Erm<&T> {
        match *self {
            Erm::Yes(ref x) => Erm::Yes(x),
            Erm::No => Erm::No,
            Erm::Maybe(ref x) => Erm::Maybe(x),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub const fn as_mut(&mut self) -> Erm<&mut T> {
        match *self {
            Erm::Yes(ref mut x) => Erm::Yes(x),
            Erm::No => Erm::No,
            Erm::Maybe(ref mut x) => Erm::Maybe(x),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    
    pub fn expect(self, msg: &str) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => panic!("called `Erm::expect` on a `No`: {}", msg),
            Erm::Maybe(x) => x,
            Erm::MaybeNot => panic!("called `Erm::expect` on a `MaybeNot`: {}", msg),
            Erm::DontKnow => panic!("called `Erm::expect` on a `DontKnow`: {}", msg),
        }
    }

    pub fn expect_sure(self, msg: &str) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => panic!("called `Erm::expect_sure` on a `No`: {}", msg),
            Erm::Maybe(_) => panic!("called `Erm::expect_sure` on a `Maybe`: {}", msg),
            Erm::MaybeNot => panic!("called `Erm::expect_sure` on a `MaybeNot`: {}", msg),
            Erm::DontKnow => panic!("called `Erm::expect_sure` on a `DontKnow`: {}", msg),
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => panic!("called `Erm::unwrap` on a `No`"),
            Erm::Maybe(x) => x,
            Erm::MaybeNot => panic!("called `Erm::unwrap` on a `MaybeNot`"),
            Erm::DontKnow => panic!("called `Erm::unwrap` on a `DontKnow`"),
        }
    }
    pub fn unwrap_sure(self) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => panic!("called `Erm::unwrap_sure` on a `No`"),
            Erm::Maybe(_) => panic!("called `Erm::unwrap_sure` on a `Maybe`"),
            Erm::MaybeNot => panic!("called `Erm::unwrap_sure` on a `MaybeNot`"),
            Erm::DontKnow => panic!("called `Erm::unwrap_sure` on a `DontKnow`"),
        }
    }
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => default,
            Erm::Maybe(x) => x,
            Erm::MaybeNot => default,
            Erm::DontKnow => default,
        }
    }
    pub fn unwrap_sure_or(self, default: T) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => default,
            Erm::Maybe(_) => default,
            Erm::MaybeNot => default,
            Erm::DontKnow => default,
        }
    }
    pub fn unwrap_or_else(self, f: impl FnOnce() -> T) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => f(),
            Erm::Maybe(x) => x,
            Erm::MaybeNot => f(),
            Erm::DontKnow => f(),
        }
    }
    pub fn unwrap_sure_or_else(self, f: impl FnOnce() -> T) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => f(),
            Erm::Maybe(_) => f(),
            Erm::MaybeNot => f(),
            Erm::DontKnow => f(),
        }
    }
    pub fn unwrap_or_default(self) -> T
    where
        T: Default,
    {
        match self {
            Erm::Yes(x) => x,
            Erm::No => T::default(),
            Erm::Maybe(x) => x,
            Erm::MaybeNot => T::default(),
            Erm::DontKnow => T::default(),
        }
    }
    pub fn unwrap_sure_or_default(self) -> T
    where
        T: Default,
    {
        match self {
            Erm::Yes(x) => x,
            Erm::No => T::default(),
            Erm::Maybe(_) => T::default(),
            Erm::MaybeNot => T::default(),
            Erm::DontKnow => T::default(),
        }
    }

    pub unsafe fn unwrap_unchecked(self) -> T {
        match self {
            Erm::Yes(x) => x,
            Erm::No => unsafe {hint::unreachable_unchecked()},
            Erm::Maybe(x) => x,
            Erm::MaybeNot => unsafe {hint::unreachable_unchecked()},
            Erm::DontKnow => unsafe {hint::unreachable_unchecked()},
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Erm<U> {
        match self {
            Erm::Yes(x) => Erm::Yes(f(x)),
            Erm::No => Erm::No,
            Erm::Maybe(x) => Erm::Maybe(f(x)),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn inspect(self, f: impl FnOnce(&T)) -> Erm<T> {
        match self {
            Erm::Yes(x) => {
                f(&x);
                Erm::Yes(x)
            }
            Erm::No => Erm::No,
            Erm::Maybe(x) => {
                f(&x);
                Erm::Maybe(x)
            }
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn map_or<U>(self, default: U, f: impl FnOnce(T) -> U) -> U {
        match self {
            Erm::Yes(x) => f(x),
            Erm::No => default,
            Erm::Maybe(x) => f(x),
            Erm::MaybeNot => default,
            Erm::DontKnow => default,
        }
    }
    pub fn map_or_else<U>(self, default: impl FnOnce() -> U, f: impl FnOnce(T) -> U) -> U {
        match self {
            Erm::Yes(x) => f(x),
            Erm::No => default(),
            Erm::Maybe(x) => f(x),
            Erm::MaybeNot => default(),
            Erm::DontKnow => default(),
        }
    }
    pub fn map_or_default<U, F>(self, f: F) -> U
    where
        U: Default,
        F: FnOnce(T) -> U,
    {
        match self {
            Erm::Yes(x) => f(x),
            Erm::No => U::default(),
            Erm::Maybe(x) => f(x),
            Erm::MaybeNot => U::default(),
            Erm::DontKnow => U::default(),
        }
    }

    pub fn ok_or(self, err: T) -> Result<T, T> {
        match self {
            Erm::Yes(x) => Ok(x),
            Erm::No => Err(err),
            Erm::Maybe(x) => Ok(x),
            Erm::MaybeNot => Err(err),
            Erm::DontKnow => Err(err),
        }
    }

    pub fn sure_or(self, err: T) -> Result<T, T> {
        match self {
            Erm::Yes(x) => Ok(x),
            Erm::No => Err(err),
            Erm::Maybe(_) => Err(err),
            Erm::MaybeNot => Err(err),
            Erm::DontKnow => Err(err),
        }
    }

    pub fn ok_or_else(self, err: impl FnOnce() -> T) -> Result<T, T> {
        match self {
            Erm::Yes(x) => Ok(x),
            Erm::No => Err(err()),
            Erm::Maybe(x) => Ok(x),
            Erm::MaybeNot => Err(err()),
            Erm::DontKnow => Err(err()),
        }
    }

    pub fn sure_or_else(self, err: impl FnOnce() -> T) -> Result<T, T> {
        match self {
            Erm::Yes(x) => Ok(x),
            Erm::No => Err(err()),
            Erm::Maybe(_) => Err(err()),
            Erm::MaybeNot => Err(err()),
            Erm::DontKnow => Err(err()),
        }
    }

    pub fn as_deref(&self) -> Erm<&T::Target>
    where
        T: Deref,
    {
        match self {
            Erm::Yes(x) => Erm::Yes(x.deref()),
            Erm::No => Erm::No,
            Erm::Maybe(x) => Erm::Maybe(x.deref()),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn as_deref_mut(&mut self) -> Erm<&mut T::Target>
    where
        T: DerefMut,
    {
        match self {
            Erm::Yes(x) => Erm::Yes(x.deref_mut()),
            Erm::No => Erm::No,
            Erm::Maybe(x) => Erm::Maybe(x.deref_mut()),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub const fn take(&mut self) -> Erm<T> {
        core::mem::replace(self, Erm::DontKnow)
    }

    pub fn ensure(self) -> Erm<T> {
        match self {
            Erm::Yes(_) => self,
            Erm::DontKnow => Erm::DontKnow,
            _ => Erm::MaybeNot,
        }
    }

    pub fn and<U>(self, other: Erm<U>) -> Erm<U> {
        match self {
            Erm::Yes(_)| Erm::Maybe(_) => other,
            Erm::No => Erm::No,
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn and_sure<U>(self, other: Erm<U>) -> Erm<U> {
        match self {
            Erm::Yes(_) => other,
            Erm::Maybe(_) => Erm::MaybeNot,
            Erm::No => Erm::No,
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn and_then<U, F>(self, f: F) -> Erm<U>
    where
        F: FnOnce(T) -> Erm<U>,
    {
        match self {
            Erm::Yes(x) => f(x),
            Erm::No => Erm::No,
            Erm::Maybe(x) => f(x),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn and_then_sure<U, F>(self, f: F) -> Erm<U>
    where
        F: FnOnce(T) -> Erm<U>,
    {
        match self {
            Erm::Yes(x) => f(x),
            Erm::No => Erm::No,
            Erm::Maybe(_) => Erm::MaybeNot,
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn filter<P>(self, predicate: P) -> Erm<T>
    where
        P: Fn(&T) -> bool,
    {
        match self {
            Erm::Yes(x) => {
                if predicate(&x) {
                    Erm::Yes(x)
                } else {
                    Erm::No
                }
            }
            Erm::Maybe(x) => {
                if predicate(&x) {
                    Erm::Maybe(x)
                } else {
                    Erm::MaybeNot
                }
            },
            _ => self,
        }
    }

    pub fn or(self, optb: Erm<T>) -> Erm<T> {
        match self {
            x @ Erm::Yes(_) | x @ Erm::Maybe(_) => x,
            _ => optb,
        }
    }

    pub fn or_sure(self, optb: Erm<T>) -> Erm<T> {
        match self {
            x @ Erm::Yes(_) => x,
            Erm::Maybe(_) => Erm::MaybeNot,
            _ => optb,
        }
    }

    pub fn or_else(self, f: impl FnOnce() -> Erm<T>) -> Erm<T> {
        match self {
            x @ Erm::Yes(_) | x @ Erm::Maybe(_) => x,
            _ => f(),
        }
    }

    pub fn or_else_sure(self, f: impl FnOnce() -> Erm<T>) -> Erm<T> {
        match self {
            x @ Erm::Yes(_) => x,
            Erm::Maybe(_) => Erm::MaybeNot,
            _ => f(),
        }
    }

    pub fn xor(self, other: Erm<T>) -> Erm<T> {
        match (self, other) {
            (a @ Erm::Yes(_), Erm::No) | (Erm::No, a @ Erm::Yes(_)) => a,
            (a @ Erm::Maybe(_), Erm::MaybeNot) | (Erm::MaybeNot, a @ Erm::Maybe(_)) => a,
            (_, Erm::DontKnow) | (Erm::DontKnow, _) => Erm::DontKnow,
            (Erm::No, Erm::No) => Erm::No,
            (Erm::Yes(_), Erm::Yes(_)) => Erm::No,
            (Erm::Maybe(_), Erm::Maybe(_)) => Erm::No,
            (Erm::MaybeNot, Erm::MaybeNot) => Erm::No,
            (Erm::No, Erm::MaybeNot) | (Erm::MaybeNot, Erm::No) => Erm::No,
            (Erm::Yes(_), Erm::Maybe(_)) | (Erm::Maybe(_), Erm::Yes(_)) => Erm::No,
            (a @ Erm::Yes(_), Erm::MaybeNot) | (Erm::MaybeNot, a @ Erm::Yes(_)) => a,
            (a @ Erm::Maybe(_), Erm::No) | (Erm::No, a @ Erm::Maybe(_)) => a,
        }
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter { inner: Item { inner: self.as_ref() } }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut { inner: Item { inner: self.as_mut() } }
    }

    pub fn insert(&mut self, value: T) -> &mut T {
        *self = Erm::Yes(value);
        
        unsafe { self.as_mut().unwrap_unchecked() }
    }
    pub fn insert_maybe(&mut self, value: T) -> &mut T {
        *self = Erm::Maybe(value);
        
        unsafe { self.as_mut().unwrap_unchecked() }
    }
    pub fn get_or_insert(&mut self, value: T) -> &mut T {
        self.get_or_insert_with(|| value)
    }
    pub fn get_or_insert_maybe(&mut self, value: T) -> &mut T {
        self.get_or_insert_with_maybe(|| value)
    }

    pub fn get_or_insert_with<F>(&mut self, f: F) -> &mut T
    where
        F: FnOnce() -> T,
    {
        match self {
            Erm::Yes(x) | Erm::Maybe(x) => x,
            _ => {
                *self = Erm::Yes(f());
                unsafe { self.as_mut().unwrap_unchecked() }
            }
        }
    }
    pub fn get_or_insert_with_maybe<F>(&mut self, f: F) -> &mut T
    where
        F: FnOnce() -> T,
    {
        match self {
            Erm::Maybe(x) | Erm::Yes(x) => x,
            _ => {
                *self = Erm::Maybe(f());
                unsafe { self.as_mut().unwrap_unchecked() }
            }
        }
    }

    pub fn get_or_insert_default(&mut self) -> &mut T
    where
        T: Default,
    {
        self.get_or_insert_with(T::default)
    }

    pub fn take_if<P>(&mut self, predicate: P) -> Erm<T>
    where
        P: FnOnce(&mut T) -> bool,
    {
        if self.as_mut().map_or(false, predicate) { self.take() } else { Erm::No }
    }

    pub const fn replace(&mut self, value: T) -> Erm<T> {
        core::mem::replace(self, Erm::Yes(value))
    }

    pub fn zip<U>(self, other: Erm<U>) -> Erm<(T, U)> {
        match (self, other) {
            (Erm::Yes(x), Erm::Yes(y)) => Erm::Yes((x, y)),
            (Erm::Maybe(x) | Erm::Yes(x), Erm::Maybe(y) | Erm::Yes(y)) => Erm::Maybe((x, y)),
            (Erm::No, _) | (_, Erm::No) => Erm::No,
            (Erm::MaybeNot, _) | (_, Erm::MaybeNot) => Erm::MaybeNot,
            (Erm::DontKnow, _) | (_, Erm::DontKnow) => Erm::DontKnow,
        }
    }

    pub fn zip_or_default<U>(self, other: Erm<U>) -> Erm<(T, U)>
    where
        T: Default,
        U: Default,
    {
        match (self, other) {
            (Erm::Yes(x), Erm::Yes(y)) => Erm::Yes((x, y)),
            (Erm::Maybe(x) | Erm::Yes(x), Erm::Maybe(y) | Erm::Yes(y)) => Erm::Maybe((x, y)),
            (Erm::Yes(x), Erm::MaybeNot) => Erm::Maybe((x, U::default())),
            (Erm::MaybeNot, Erm::Yes(y)) => Erm::Maybe((T::default(), y)),
            (Erm::No, _) | (_, Erm::No) => Erm::No,
            (Erm::MaybeNot, _) | (_, Erm::MaybeNot) => Erm::MaybeNot,
            (Erm::DontKnow, _) | (_, Erm::DontKnow) => Erm::DontKnow,
        }
    }

    pub fn zip_with<U, F>(self, other: Erm<U>, f: F) -> Erm<T>
    where
        F: FnOnce(T, U) -> T,
    {
        match (self, other) {
            (Erm::Yes(x), Erm::Yes(y)) => Erm::Yes(f(x, y)),
            (Erm::Maybe(x) | Erm::Yes(x), Erm::Maybe(y) | Erm::Yes(y)) => Erm::Maybe(f(x, y)),
            (Erm::No, _) | (_, Erm::No) => Erm::No,
            (Erm::MaybeNot, _) | (_, Erm::MaybeNot) => Erm::MaybeNot,
            (Erm::DontKnow, _) | (_, Erm::DontKnow) => Erm::DontKnow,
        }
    }
}

impl <T, U> Erm<(T, U)> {
    pub fn unzip(self) -> (Erm<T>, Erm<U>) {
        match self {
            Erm::Yes((x, y)) => (Erm::Yes(x), Erm::Yes(y)),
            Erm::Maybe((x, y)) => (Erm::Maybe(x), Erm::Maybe(y)),
            Erm::No => (Erm::No, Erm::No),
            Erm::MaybeNot => (Erm::MaybeNot, Erm::MaybeNot),
            Erm::DontKnow => (Erm::DontKnow, Erm::DontKnow),
        }
    }
}

impl<T> Erm<&T> {
    pub fn copied(self) -> Erm<T>
    where
        T: Copy,
    {
        match self {
            Erm::Yes(&x) => Erm::Yes(x),
            Erm::No => Erm::No,
            Erm::Maybe(&x) => Erm::Maybe(x),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn cloned(self) -> Erm<T>
    where
        T: Clone,
    {
        match self {
            Erm::Yes(x) => Erm::Yes(x.clone()),
            Erm::No => Erm::No,
            Erm::Maybe(x) => Erm::Maybe(x.clone()),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }
}

impl<T> Erm<&mut T> {
    pub fn copied(self) -> Erm<T>
    where
        T: Copy,
    {
        match self {
            Erm::Yes(&mut x) => Erm::Yes(x),
            Erm::No => Erm::No,
            Erm::Maybe(&mut x) => Erm::Maybe(x),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }

    pub fn cloned(self) -> Erm<T>
    where
        T: Clone,
    {
        match self {
            Erm::Yes(x) => Erm::Yes(x.clone()),
            Erm::No => Erm::No,
            Erm::Maybe(x) => Erm::Maybe(x.clone()),
            Erm::MaybeNot => Erm::MaybeNot,
            Erm::DontKnow => Erm::DontKnow,
        }
    }
}

impl<T, E> Erm<Result<T, E>> {
    pub fn transpose(self) -> Result<Erm<T>, E> {
        match self {
            Erm::Yes(Ok(x)) => Ok(Erm::Yes(x)),
            Erm::Yes(Err(e)) => Err(e),
            Erm::No => Ok(Erm::No),
            Erm::Maybe(Ok(x)) => Ok(Erm::Maybe(x)),
            Erm::Maybe(Err(e)) => Err(e),
            Erm::MaybeNot => Ok(Erm::MaybeNot),
            Erm::DontKnow => Ok(Erm::DontKnow),
        }
    }
}

impl<T> Default for Erm<T> {
    fn default() -> Self {
        Erm::DontKnow
    }
}

impl<T> IntoIterator for Erm<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { inner: Item { inner: self } }
    }
}

impl<'a, T> IntoIterator for &'a Erm<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter { inner: Item { inner: self.as_ref() } }
    }
}

impl<'a, T> IntoIterator for &'a mut Erm<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        IterMut { inner: Item { inner: self.as_mut() } }
    }
}

impl<T> From<T> for Erm<T> {
    fn from(value: T) -> Self {
        Erm::Yes(value)
    }
}

impl<'a, T> From<&'a Erm<T>> for Erm<&'a T> {
    fn from(value: &'a Erm<T>) -> Self {
        value.as_ref()
    }
}

impl<'a, T> From<&'a mut Erm<T>> for Erm<&'a mut T> {
    fn from(value: &'a mut Erm<T>) -> Self {
        value.as_mut()
    }
}

impl<T> Into<Option<T>> for Erm<T> {
    fn into(self) -> Option<T> {
        match self {
            Erm::Yes(x) => Some(x),
            Erm::No => None,
            Erm::Maybe(x) => Some(x),
            Erm::MaybeNot => None,
            Erm::DontKnow => None,
        }
    }
}

impl<T> From<Option<T>> for Erm<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(x) => Erm::Yes(x),
            None => Erm::No,
        }
    }
}

#[derive(Debug, Clone)]
struct Item<A> {
    inner: Erm<A>
}

impl <A> Iterator for Item<A> {
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.take().into()
    }
}

impl <A> DoubleEndedIterator for Item<A> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.take().into()
    }
}

impl<A> ExactSizeIterator for Item<A> {
    fn len(&self) -> usize {
        match self.inner {
            Erm::Yes(_) | Erm::Maybe(_) => 1,
            Erm::No | Erm::MaybeNot | Erm::DontKnow => 0,
        }
    }
}

impl<A> FusedIterator for Item<A> {}
// unsafe impl<A> TrustedLen for Item<A> {}

#[derive(Debug)]
pub struct Iter<'a, A: 'a> {
    inner: Item<&'a A>,
}

impl<'a, A> Iterator for Iter<'a, A> {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<'a, A> DoubleEndedIterator for Iter<'a, A> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back()
    }
}

impl<A> ExactSizeIterator for Iter<'_, A> {}

impl <A> FusedIterator for Iter<'_, A> {}

impl<A> Clone for Iter<'_, A> {
    fn clone(&self) -> Self {
        Iter {
            inner: self.inner.clone(),
        }
    }
}

pub struct IterMut<'a, A: 'a> {
    inner: Item<&'a mut A>,
}

impl<'a, A> Iterator for IterMut<'a, A> {
    type Item = &'a mut A;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<'a, A> DoubleEndedIterator for IterMut<'a, A> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back()
    }
}

impl<A> ExactSizeIterator for IterMut<'_, A> {}
impl <A> FusedIterator for IterMut<'_, A> {}

pub struct IntoIter<A> {
    inner: Item<A>,
}

impl<A> Iterator for IntoIter<A> {
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<A> DoubleEndedIterator for IntoIter<A> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back()
    }
}

impl<A> ExactSizeIterator for IntoIter<A> {}
impl<A> FusedIterator for IntoIter<A> {}


