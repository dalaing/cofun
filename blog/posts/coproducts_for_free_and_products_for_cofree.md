---
title: Coproducts for free, and products for cofree
published: 2015-06-29 09:00:00+10:00
---

# Coproducts for free monads

In the last two posts, we have [built a DSL from a free monad and the corresponding interpreter from a cofree comonad](/posts/free_and_cofree.html) and [cleaned up the interpreter with comonad transformers](/posts/monad_transformers_and_comonad_transformers.html).

The comonad transformers helped to factor out common concerns - in this case, passing in configuration and handling state.

We can separate out our concerns even further than this.

The underlying functor for our DSL is
```haskell
data AdderF k =
    Add Int (Bool -> k)
  | Clear k
  | Total (Int -> k)
```

You might have noticed that the three constructors are independent of one another with respect to the `Functor` instance and the free monad.
You might also have read [Data types a la carte](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4131).

As an aside, if you haven't read 'Data types a la carte' before you should give it a go.
It's really well written, and if you've followed the series up to this point then you're probably ready for it.
I tried to read it very early on in my Haskell journey and ended up slowly backing away, but I'm glad I doubled back around to it eventually.

Anyhow.

We can break the independent parts out:
```haskell
data AddF k = Add Int (Bool -> k)

data ClearF k = Clear k

data TotalF k = Total (Int -> k)
```
and we can write `Functor` instances for these:
```haskell
instance Functor AddF where
  fmap f (Add x k) = Add x (f . k)

instance Functor ClearF where
  fmap f (Clear k) = Clear (f k)

instance Functor TotalF where
  fmap f (Total k) = Total (f . k)
```

We can now recover `AdderF` by using [`Sum`](https://hackage.haskell.org/package/transformers/docs/Data-Functor-Sum.html), which makes a `Functor` from the sum of two other `Functors`.

The relevant bits of `Sum` are
```haskell
data Sum f g a = InL (f a) | InR (g a)

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (InL x) = InL (fmap f x)
  fmap f (InR x) = InR (fmap f x)
```
and we end up with
```haskell
{-# LANGUAGE TypeOperators #-}

-- we introduce a type operator to match "Data types a la carte" and Dan Piponi's post on free and cofree
type f :+: g = Sum f g

type AdderF = AddF :+: ClearF :+: TotalF
```
which will behave in the same manner the old `AdderF` as far as the `Functor` instance - and hence the free monad - is concerned.

## Classier coproducts

With some help from the typeclass machinery introduced in "Data types a la carte", we can make our combinators more flexible.

The relevant machinery is as follows:
```haskell
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InR . inj
```

This allows us to set up a constraint `f :<: g`, which asserts that `f` is somewhere in the sum of functors `g`.
Where that constraint holds we can also convert an `f` into a `g` with `inj`.

As an aside, Phil Wadler [noted](http://wadler.blogspot.com.au/2008/02/data-types-la-carte.html) that the above machinery is asymmetric - which means that while
```haskell
f :<: d :+: e :+: f :+: g
```
is fine
```haskell
f :<: d :+: (e :+: f) :+: g
```
is not.

There are a few solutions posted in the comments section of that post.
I'm not sure what the current best practice is on that front.
I'll be looking into it later, but if anyone has any pointers or thoughts on the topic I'd love to hear about it.

The above machine is fine for this post, as long as we're reasonably careful.

With the new tool in the toolbox, we can now do the following:
```haskell
add :: (MonadFree f m, AddF :<: f) => Int -> m Bool
add x = liftF . inj $ Add x id
```

```haskell
clear :: (MonadFree f m, ClearF :<: f) => m ()
clear = liftF . inj $ Clear ()
```

```haskell
total :: (MonadFree f m, TotalF :<: f) => m Int
total = liftF . inj $ Total id
```

These can be put in separate modules and imported as needed by client code.

That's probably not so impressive in this case.
As a more impressive example, we could build up separate DSLs for authentication, database access and logging, and then mix and match to get whatever custom combination we needed, with the ability to make use of  any code we'd built up on top of the individual DSLs (or combinations of those DSLs).

To see how this looks, we can update `findLimit` to work in a larger number of contexts:
```haskell
findLimit :: (MonadFree f m, AddF :<: f, ClearF :<: f, TotalF :<: f) => m Int
findLimit = do
   -- capture the old count
   t <- total
   -- clear the count and the state
   clear
   r <- execStateT findLimit' 0
   -- restore the old count
   clear
   _ <- add t
   return r

findLimit' :: (MonadFree f m, AddF :<: f) => StateT Int m ()
findLimit' = do
  r <- lift $ add 1
  when r $ do
    modify (+ 1)
    findLimit'
```

While it looks pretty similar to how it did before, we can now use `findLimit` with a free monad that has additional components in its underlying functor.

We could also have broken things up further, with
```haskell
reset :: (MonadFree f m, ClearF :<: f, TotalF :<: f) => m Int
reset = do
   -- capture the old count
   t <- total
   -- clear the count and the state
   clear
   return t
```
and
```haskell
restore :: (MonadFree f m, AddF :<: f, ClearF :<: f) => Int -> m ()
restore = do
   -- restore the old count
   clear
   _ <- add t
   return ()
```

Restricting our types down to just the things that we need is great - it provides more info about what the functions can and cannot do, and it means that there are less moving parts and so less ways to write the functions incorrectly.

# Products for cofree comonads

That's all well and good for the DSL, but we now need to update our interpreter to work with it.

Remember that we had an underlying functor for our interpreter:
```haskell
data CoAdderF k = CoAdderF {
    addH   :: Int -> (Bool,k)
  , clearH :: k
  , totalH :: (Int,k)
  }
```
and a pairing between our DSL and interpreter functors

We can factor those out into independent pieces as well.

`CoAdderF` gets broken in a similar manner to what we did with `AdderF`:
```haskell
data CoAddF k = CoAdd (Int -> (Bool, k))

data CoClearF k = CoClear k

data CoTotalF k = CoTotal (Int, k)
```
and we can easily make the corresponding `Functor` instances:
```haskell
instance Functor CoAddF where
  fmap f (CoAdd a) = CoAdd (fmap (fmap f) a)

instance Functor CoClearF where
  fmap f (CoClear k) = CoClear (f k)

instance Functor CoTotalF where
  fmap f (CoTotal t) = CoTotal (fmap f t)
```

In the same way that we used the `Sum` to recover `AdderF`, we can use [`Product`](https://hackage.haskell.org/package/transformers/docs/Data-Functor-Sum.html) to recover `CoAdderF`.

The relevant bits of `Product` are
```haskell
data Product f g a = Pair (f a) (g a)

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap h (Pair f g) = Pair (fmap h f) (fmap h g)
```
which gives us
```haskell
{-# LANGUAGE TypeOperators #-}

-- we introduce a type operator to match "Data types a la carte" and Dan Piponi's post on free and cofree
type f :*: g = Product f g

type CoAdderF = CoAddF :*: CoClearF :*: CoTotalF
```

Again, the new version of `CoAdderF` will behave in the same way as the old version as far as we're concerned.

Our old interpreter functions had types:
```haskell
coAdd   :: (ComonadEnv Int w, ComonadStore Int w) => w a -> Int -> (Bool, w a)
coClear :: ComonadStore Int w => w a -> w a
coTotal :: ComonadStore Int w => w a -> (Int, w a)
```
but the new ones have a constructor wrapping up most of the details
```haskell
coAdd   :: (ComonadEnv Int w, ComonadStore Int w) => w a -> CoAddF (w a)
coClear :: ComonadStore Int w => w a -> CoClearF (w a)
coTotal :: ComonadStore Int w => w a -> CoTotalF(w a)
```

Aside from the changes associated with those new constructors, the implementations don't change much:
```haskell
coAdd :: (ComonadEnv Int w, ComonadStore Int w) => w a -> CoAddF (w a)
coAdd w = CoAdd $ \x ->
  let
    count = pos w
    limit = ask w
    count' = count + x
    test = count' <= limit
    next = if test then count' else count
  in
    (test, seek next w)
```

```haskell
coClear :: ComonadStore Int w => w a -> CoClearF (w a)
coClear = CoClear . seek 0
```

```haskell
coTotal :: ComonadStore Int w => w a -> CoTotalF (w a)
coTotal w = CoTotal (pos w, w)
```

Just like the components of our DSL, the components of the interpreter can all be placed in different modules and mixed and matched as we like.

We just need something to stitch these together.
Since `coiter` takes a function `a -> f a` and `coiterT` takes a function `w a -> f (w a)`, we should be able to do our stitching in terms of `a -> f a` for the various components and have it work for either `coiter` or `coiterT`.

What we need is something to combine two of these functions into a product:
```haskell
(*:*) :: (Functor f, Functor g) => (a -> f a) -> (a -> g a) -> a -> (f :*: g) a
(*:*) = liftA2 Pair
```
which we can use to update the definition of `mkCoAdder`:
```haskell
type CoAdderF = CoAddF :*: CoClearF :*: CoTotalF

mkCoAdder :: Int -> Int -> CoFreeT CoAdderF (StoreT Int (EnvT Int Identity)) ()
mkCoAdder limit count =
    coiterT next start
  where
    next = coAdd *:* coClear *:* coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
```

# Pairings

The last piece of the puzzle is to establish a `Pairing` between `AdderF` and `CoAdderF`.
Previously, this was
```haskell
instance Pairing CoAdderF AdderF where
  pair f (CoAdderF a _ _) (Add x k) = pair f (a x) k
  pair f (CoAdderF _ c _) (Clear k) = f c k
  pair f (CoAdderF _ _ t) (Total k) = pair f t k
```

There are pairings between `Sum` and `Product` functors if there are pairings between their components:
```haskell
instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (InL x) (Pair a _) = pair p x a
  pair p (InR x) (Pair _ b) = pair p x b

instance (Pairing f f', Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (Pair a _) (InL x) = pair p a x
  pair p (Pair _ b) (InR x) = pair p b x
```

This means that we only need to provide pairings between the corresponding functors from the DSL and interpreter functors:
```haskell
instance Pairing CoAddF AddF where
  pair f (CoAdd a) (Add x k) = pair f (a x) k

instance Pairing CoClearF ClearF where
  pair f (CoClear c) (Clear k) = f c k

instance Pairing CoTotalF TotalF where
  pair f (CoTotal t) (Total k) = pair f t k
```
and we'll be back to where we were:
```haskell
type AdderF   = AddF :+: ClearF :+: TotalF
-- There is a Functor instance for AdderF if there are
-- Functor instances for each of AddF, ClearF and TotalF

type CoAdderF = CoAddF :*: CoClearF :*: CoTotalF
-- There is a Functor instance for CoAdder if there are
-- Functor instances for each of CoAddF, CoClearF and CoTotalF

-- There is a Pairing instance for CoAdderF an AdderF if there are
-- Pairing instances for each of AddF / CoAddF, ClearF / CoClearF and TotalF / CoTotalF
```

Everything should work out as before, and we've gained the ability to mix and match functionality in both the DSL and the interpreter.

Even though we have the parts of the interpreter separated out, they still interact via the underlying comonad transformers.
This provides another axis for combination and reuse.

For example, you can write reusable code on top of component `X`, you can write reusable code on top of component `Y`, and you can also write reusable code on top of the combination of components `X` and `Y` and a `ComonadStore Int w` context.

Lots of fun to be had.

# Conclusion and open questions

We now have a decent separation of concerns for our DSL and interpreter, and the ability to mix and match DSLs and interpreters together.

It also means that we can write functions that work with the DSL or interpreter in a context where we only have access to the components that we really need.
This increases the scope for reuse and decreases the scope for writing misbehaving code, and I'm a fan of both of those.

I'm still interested in how to do better with the "Data types a la carte" machinery.
With the current machinery, we need to make sure that our `Sum`s and `Product`s have the same components in the same order.
It feels like it should be possible to do significantly better than this, such that:

* the components are guaranteed to be unique and the order doesn't matter (effectively a Set)
    * this should also deal with the current problem of asymmetry
* if the `Sum` components are a subset of the `Product` components we can automatically create a `Pairing` from the `Pairing`s between the components.

I'll probably tinker with this eventually, but if someone gets there before I do I'll be pretty happy.

I'm also curious about whether we can [go even further than Sum and Product](http://stackoverflow.com/a/21395817), although I'm still not clear on how far that can be pushed in this context to make things more useful.

There's still a few posts to come before I've covered everything I mentioned in the talk.

So far, none of the interpreters we've defined have done any IO.  The next post will look at our options for dealing with effects in our DSLs and interpreters.

[Questions? Comments?](https://www.reddit.com/r/haskell/comments/3beiti/coproducts_for_free_and_products_for_cofree/)
