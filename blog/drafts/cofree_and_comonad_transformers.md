---
title: Cofree and comonad transformers
published: 2015-05-24 19:00:00+10:00
---

# The story so far

[Previously](/posts/free_and_cofree.html) we put together a DSL using the free monad and a corresponding interpreter using the cofree comonad.

Here is our code for the interpreter:
```haskell
type CoAdder = Cofree CoAdderF

coAdd :: (Int, Int) -> Int -> (Bool, (Int, Int))
coAdd (limit, count) x = (test, (limit, next))
  where
    count' = count + x
    test = count' <= limit
    next = if test then count' else count

coClear :: (Int, Int) -> (Int, Int)
coClear (limit, _) = (limit, 0)

coTotal :: (Int, Int) -> (Int, (Int, Int))
coTotal (limit, count) = (count, (limit, count))

mkCoAdder :: Int -> Int -> CoAdder (Int, Int)
mkCoAdder limit count = coiter next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = (limit, count)
```

There are a few different things going on here.

# Cleaning up with comonad transformers

There are two parts to what we're doing that we can factor out so that we don't have to manage them ourselves.

We are making use of `count` as a kind of state.
If we were working in a monadic context, we'd reach for eitehr the `State` monad or `StateT` monad transformer.
The comonad transformer equivalent is `Store/StoreT`.

We are making use of the `limit` as a kind of environment.
Where a monadic version of the code would use `Reader/ReaderT`, the comonadic version will use `Env/EnvT`.

```haskell
class MonadTrans t where
  lift :: w a -> t w a
```

```haskell
class ComonadTrans t where
  lower :: t w a -> w a
```

## Setting up the stack

```haskell
type CoAdder = CoAdderT Base
type Base = StoreT Int (EnvT Int Identity)
type CoAdderT = CofreeT CoAdderF

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count = coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
```

## Introducing `StoreT`

TODO storet definition and operations, what they mean

```haskell
coClear :: Base a -> Base a
coClear = seek 0

coTotal :: Base a -> (Int, Base a)
coTotal w = (pos w, w)
```

## Introducting `EnvT`

TODO envt definition and operations, what they mean

```haskell
coAdd :: Base a -> Int -> (Bool, Base a)
coAdd w x = (test, seek next w)
  where
    count = pos  w
    limit = ask . lower $ w
    count' = count + x
    test = count' <= limit
    next = if test then count' else count
```

## The hard way

This was pretty easy, but it gets a little trickier if we change the order of the transformer stack.

```haskell
type CoAdder = CoAdderT Base
type Base = EnvT Int (StoreT Int Identity)
type CoAdderT = CofreeT CoAdderF

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = EnvT limit . flip StoreT count . Identity $ const ()
```

Since `seek s` is defined as `peek s . duplicate`, we need some fancier footwork to update the value in the `StoreT`:
```haskell
coClear :: Base a -> Base a
coClear = peek 0 . lower . duplicate
```

Getting access to `pos` just requires that we `lower` into the `StoreT` first:
```haskell
coTotal :: Base a -> (Int, Base a)
coTotal w = (pos . lower $ w), w)

coAdd :: Base a -> Int -> (Bool, Base a)
coAdd w x = (test, peek next . lower . duplicate $ w)
  where
    count = pos . lower $ w
    limit = ask w
    count' = count + x
    test = count' <= limit
    next = if test then count' else count
```

# From `transformers` style to `mtl` style

There are different schools of thoughts on whether to use `transformers` style monad transformers - in which you have concrete types and you use explicit lifts - or `mtl` style monad transformers - in which you use typeclass constraints and don't have to lift anything.

I'm not going to take a strong stance here.
With `transformers` your code is explicit and you can have multiple transformers of the same type in your stack.
With `mtl` you can't have multiple transformers of the same type, but you can write code that is decoupled from the concrete stack you end up using.

I lean slightly towards working with `mtl` style transformers, since I like the ease with which I can use it for prototyping and putting together pieces that I might reuse - even if the reuse is just between now and the next refactoring.

Anyhow.

What we have so far demonstrates `transformers` style comonad transformers.  It might be instructive to show off the differences between that and `mtl` style comonad transformers.

The cases for `coClear` and `coTotal` don't change except for the type signatures, since `StoreT` is at the top of our stack.
```haskell
coClear :: ComonadStore Int w => w a -> w a
coClear = seek 0

coTotal :: ComonadStore Int w => w a -> (Int, w a)
coTotal w = (pos w, w)
```

For `coAdd` we make a similar change to the type signature, and we drop the explicit `lower`, since the `ComonadEnv` constraint makes `ask` available to us no matter where it is in the stack
```haskell
coAdd :: (ComonadEnv Int w, ComonadStore Int w) => w a -> Int -> (Bool, w a)
coAdd w x = (test, seek next w)
  where
    count = pos w
    limit = ask w
    count' = count + x
    test = count' <= limit
    next = if test then count' else count
```

The mkCoadder code doesn't change at all - and more importantly, the `coClear`, `coTotal` and `coAdd` methods don't change even if we change the order in which we stack the transformers.

Both
```haskell
type CoAdderT = CofreeT CoAdderF
type CoAdder = CoAdderT (StoreT Int (EnvT Int Identity))

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
```
and
```haskell
type CoAdderT = CofreeT CoAdderF
type CoAdder = CoAdderT (EnvT Int (StoreT Int Identity))

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = EnvT limit . flip StoreT count . Identity $ const ()
```
will work without adjustments.

# Conclusion

We've now factored out the state and evnironment from the interpreter, but there are still aspects of both the DSL and the interpreter which are more strongly coupled than they need to be.
