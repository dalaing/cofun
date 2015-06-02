---
title: Coproducts for free, and products for cofree
published: 2015-05-25 19:00:00+10:00
---

# Coproducts for free monads

The underlying functor for our DSL is
```haskell
data AdderF k =
    Add Int (Bool -> k)
  | Clear k
  | Total (Int -> k)
```

You might have noticed that these are independent of one another as far as the functor instance and the free monad go.  Or you might have read [Datatypes a la carte](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4131).

As an aside, if you haven't read 'Datatypes a la carte' before you should give it a go.
It's really well written, and if you've followed up to this point then you're probably ready for it.
I tried to read it very early on in my Haskell journey and ended up slowly backing away, but I'm glad I doubled back around to it eventually.

You can break the independent parts out
```haskell
data AddF k = Add Int (Bool -> k)

data ClearF k = Clear k

data TotalF k = Total (Int -> k)
```
and you can write functor instances for these
```haskell
instance Functor AddF where
  fmap f (Add x k) = Add x (f . k)

instance Functor ClearF where
  fmap f (Clear k) = Clear (f k)

instance Functor TotalF where
  fmap f (Total k) = Total (f . k)
```

Using the `Sum` functor, we can recover the old `AdderF`.
[Sum](https://hackage.haskell.org/package/transformers/docs/Data-Functor-Sum.html) comes from `transformers`, and makes a `Functor` from the sum of two `Functors`.

```haskell
{-# LANGUAGE TypeOperatoes #-}

-- we introduce a type operator to match "Datatypes a la carte"
type f :+: g = Sum f g

type AdderF = AddF :+: ClearF :+: TotalF
```

And, with some help from the typeclass machinery introduced in "Datatypes a la carte", we can make our combinators more flexible.
```haskell
```
TODO explain why this is good / useful

This works with `findLimit`
```haskell
```
but we can make that more flexible as well.
```haskell
```

# Products for cofree comonads

Recall that we had an underlying functor for our interpreter
```haskell
data CoAdderF k = CoAdderF {
    addH   :: Int -> (Bool,k)
  , clearH :: k
  , totalH :: (Int,k)
  }
```
and a pairing between our DSL and interpreter functors
```haskell
instance Pairing CoAdderF AdderF where
  pair f (CoAdderF a _ _) (Add x k) = pair f (a x) k
  pair f (CoAdderF _ c _) (Clear k) = f c k
  pair f (CoAdderF _ _ t) (Total k) = pair f t k
```

We can factor those out into independent peices as well:
```haskell
data CoAddF k = CoAdd (Int -> (Bool, k))

data CoClearF k = CoClear k

data CoTotalF k = CoTotal (Int, k)
```
and provide the `Functor` instances:
```haskell
instance Functor CoAddF where
  fmap f (CoAdd a) = CoAdd (fmap (fmap f) a)

instance Functor CoClearF where
  fmap f (CoClear k) = CoClear (f k)

instance Functor CoTotalF where
    fmap f (CoTotal t) = CoTotal (fmap f t)
```

In the same way that we used the `Sum` functor for the DSL, we can use the `Product` functor to recover the old `CoAdderF`.

[Product](https://hackage.haskell.org/package/transformers/docs/Data-Functor-Product.html) comes from `transformers`, and makes a `Functor` from the product of two `Functors`.

```haskell
{-# LANGUAGE TypeOperatoes #-}

-- we introduce a type operator to match "Datatypes a la carte"
type f :*: g = Product f g

type CoAdderF = CoAddF :*: CoClearF :*: CoTotalF
```

TODO mkCoAdder

TODO explain cross talk via underlying transformers

# Pairing coproducts and products

There are pairings between `Sum` and `Product` functors if there are pairings between their components
```haskell
instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (LeftF x) (a :*: _) = pair p x a
  pair p (RightF x) (_ :*: b) = pair p x b

instance (Pairing f f', Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (a :*: _) (LeftF x) = pair p a x
  pair p (_ :*: b) (RightF x) = pair p b x
```

This means that we only need to provide pairings between the corresponding functors from the DSL and interpreter functors, and we'll be back to where we were:
```haskell
instance Pairing CoAddF AddF where
  pair f (CoAdd a) (Add x k) = pair f (a x) k

instance Pairing CoClearF ClearF where
  pair f (CoClear c) (Clear k) = f c k

instance Pairing CoTotalF TotalF where
    pair f (CoTotal t) (Total k) = pair f t k
```

# Conclusion

TODO why this is good

TODO opens the door to good things, allows a lighter touch, networking related results coming soon

TODO at end
- can we use products in a meaningful way in free? timing and auth?
- can we use compose and friends to make things interestings?
