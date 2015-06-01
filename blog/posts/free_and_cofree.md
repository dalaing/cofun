---
title: Free for DSLs, cofree for interpreters
published: 2015-06-01 22:00:00+10:00
---

# Free for DSLs, cofree for interpreters

This is the first post in a series of posts, which will cover the material from a talk I gave at YOW! Lambda Jam, titled "Cofun with Cofree Comonads".

The slides, these posts, and the associated code are in the [github repository](https://github.com/dalaing/cofun).

## An introduction to free monads for DSLs

I'm not going to be getting into the underlying theory of free monads to any extend in these posts.

Instead I'll be focusing on the use of free monads in order to build something like a DSL (domain-specific language).

Gabriel Gonzales has written some great posts on free monads [here](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html) and [here](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html).
If you haven't come across free monads before, I recommend reading those posts before continuing, because I'm most likely going to butcher the explanation in my rush to get to cofree.


### Free

The `Free` datatype has two constructors:
```haskell
data Free f a =
    Pure a
  | Free (f (Free f a))
```

If you squint, this can be viewed as a kind of tree.

The `Pure` constructor can be viewed as the leaves of a tree.
The `Free` constructor can be viewed as the branches.  If `f` is a `Functor` and we use the functors-as-containers analogy, then we have a container of `Free f a`s.

Additionally, if `f` is a `Functor` then we can define a `Monad` instance for `Free f`:
```haskell
instance Functor f => Monad (Free f) where
  return x = Pure x
  (Pure r) >>= f = f r
  (Free x) >>= f = Free (fmap (>>= f) x)
```

In the above instance

- `return` turns a pure value into a leaf of the tree
- `>>=` takes a function for turning pure values into trees, and subsitutes the leaves with the trees which result from running that function on the values in the leaves

### Our toy DSL

It turns out this is pretty handy for defining and working with DSLs, so lets put a toy DSL together in order to see what this actually gives us.

The DSL will be used to track the running total as we add to, clear, and query the total.

First we need to define the underlying functor:

```haskell
data AdderF k =
    Add Int (Bool -> k)
  | Clear k
  | Total (Int -> k)
```

The type parameter `k` can be thought of as "what to do next".

The DSL will have three commands

- add an `Int` to the total, after which we get hold of a `Bool` which indicates if we can continue
    - if the `Bool` is `true`, the `Int` has been added to the total
    - if the `Bool` is `false`, the `Int` would have overflowed the counter the service is using to track the total, so the total is left unchanged
    - we can use that `Bool` to determine what we will do next
- clear the total, and then move on to the next DSL action.
- ask for the total
    - we can choose our next action based on the `Int` that we get back.

This has a functor instance:
```haskell
instance Functor AdderF where
  fmap f (Add x k) = Add x (f . k)
  fmap f (Clear k) = Clear (f k)
  fmap f (Total k) = Total (f . k)
```
but we could have turned on the `DeriveFunctor` language extension and gotten this for free.

Now we have what we need to create a monad for manipulating our DSL:
```haskell
type Adder a = Free AdderF a
```

At this point we have lost our `k` parameter from before.

If we unfold the definitions a little, we end up with the following:

```haskell
type Adder a =
    Pure a
  | Free (Add Int (Bool -> Adder a))
  | Free (Clear (Adder a))
  | Free (Total (Int -> Adder a)
```

If we were thinking of `k` as "what to do next", we see that the thing that we do after carrying out an action in the `Adder` monad is either
- return a value
- carry out another action in the `Adder` monad

We can also add some extra effects into the mix by using `FreeT`:
```haskell
type AdderT m a = FreeT AdderF m a
```

### Combinators for the DSL

At the moment this is a bit inconvenient to work with, so we use `liftF` from the `free` package to build the combinators for working in our language from the data type:

```haskell
add :: Int -> Adder Bool
add x = liftF $ Add x id

clear :: Adder ()
clear = liftF $ Clear ()

total :: Adder Int
total = liftF $ Total id
```

We can generalize this in order to work with other effects:
```haskell
type AdderT m a = FreeT AdderF m a

add :: Monad m => Int -> AdderT m Bool
add x = liftF $ Add x id

clear :: Monad m => AdderT m ()
clear = liftF $ Clear ()

total :: Monad m => AdderT m Int
total = liftF $ Total id
```

If we instantiate `m` as `Identity` then the two are equivalent.

And that's it.  We know have our own monad for manipulating the DSL, which means we can use `do`-notation, we can sequence and traverse these commands, and generally go to town with all of the existing machinery we have in the Haskell ecosystem for building things with monads.

We also haven't made any reference to how this DSL will be interpreted.
That gives us a lot of freedom, which also allows us to build different interpreters for testing and for production without changing our DSL or anything built on top of it.

### Building some extensions

We know that the counter tracking the total _can_ overflow, but we don't have the ability to ask what it's limit is.

Thankfully, we have everything we need to write one.

The `findLimit` function captures and restores the total, so that we'll play nicely with out functions that work with the `Adder` monad, and calls out to a helper function to actually find the limit:
```haskell
findLimit :: Adder Int
-- or if we want to be more general:
--   findLimit :: Monad m => AdderT m Int
findLimit = do
  -- capture the old count
  t <- total
  -- clear the count
  clear
  -- seek out the limit
  r <- execStateT findLimit' 0
  -- restore the old count
  clear
  _ <- add t
  -- return the result
  return r
```

As `Adder` is a monad, we can put it in a stack of monad transformers.
In this case we use `StateT` to keep track of how much we've added to the total so far.

We have `execStateT findLimit' 0` in `findLimit` which will start the state at `0` and return whatever it ends up at when we finish with `findLimit'`, so we just need to increment both our total and our state until we overflow and all should be well.
```haskell
findLimit' :: StateT Int Adder ()
findLimit' = do
  -- add 1 to the total
  r <- lift $ add 1
  -- check for overflow
  when r $ do
    -- if no overflow, add to our state counter ...
    modify (+ 1)
    -- and continue
    findLimit'
```

### Ad-hoc interpretation of our free monad ###

At this point, some folks would build an interpreter for their DSL that may look a little like this:
```haskell
interpret :: Monad m => Limit -> Count -> AdderT m r -> m r
interpret limit count a = do
  mr <- runFreeT a
  case mr of
   Pure r -> return r
   Free (Add x k) -> do
     let count' = x + count
     let test = count' <= limit
     let next = if test then count' else count
     interpret limit next (k test)
   Free (Clear k) ->
     interpret limit 0 k
   Free (Total k) ->
     interpret limit count (k count)
```

We can encapsulate the recursion with things like `iter`, `iterT` and `iterTM`, and I've written `iterTTM` in the accompanying to show how that works in this case.

That's fine, but I think we can do better.

## Cofree

If you're not familiar with comonads, I highly recommend another of Gabriel Gonzalez's [posts](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html).
I'm also writing something based on the rushed explanation I gave in my talk, and will add a link here once it is ready.

We won't need much familiarity with comonads now, but it'll probably come in handy for subsequent posts.

Where `Free` was a sum type, `Cofree` is the product type:
```haskell
data Cofree f a = a :< f (Cofree f a)
```

You can also view this as a kind of tree, except we've got values at the branching points instead of at the leaves.
This make `Cofree` handy for working with interpreters that run forever, since we can lazily build an infinite `Cofree` and then travel through the tree as we like.

Again, if `f` is a functor then we have a `Comonad` for `Cofree f`:
```haskell
instance Functor f => Comonad (Cofree f)
  extract (a :< _) = a
  duplicate c@(_ :< fs) = Cofree c (fmap duplicate fs)
```

### An interpreter for our DSL

We're now specifying an interpreter rather than a DSL.
At any point in time, the interpreter needs to be able to handle _any_ of our DSL actions.
Where we had a sum type of three actions in the DSL, we now have a product type of three handlers in the interpreter.

```haskell
data CoAdderF k = CoAdderF {
    addH   :: Int -> (Bool, k)
  , clearH :: k
  , totalH :: (Int, k)
  }
```

The type parameter `k` represents the handlers that we'll be using once we've interpreted the next action.

In order to make a `Comonad` out of this we need a `Functor` instance.
We could just use `DeriveFunctor`, but it's easy enough to write out here:
```haskell
instance Functor CoAdderF where
  fmap f (CoAdderF a c t) = CoAdderF
    (fmap (fmap f) a)
    (f c)
    (fmap f t)
```

### Providing an interpretation for our interpreter ###

We can use `coiter` to lazily build up a `Cofree` value.

```haskell
coiter :: Functor f => (a -> f a) -> a -> Cofree f a
```

We just need a seed value and a function which will take us from one level to the next.

This will expand
```haskell
coiter next start
```
to
```haskell
start :< coiter next <$> next start
```
to
```haskell
start :< next start :< coiter next <$> (next . next $ start)
```
and so on.

So armed, we begin the definition of our interpreter:
```haskell
type Limit = Int
type Count = Int

mkCoAdder :: Limit -> Count -> CoAdder (Limit, Count)
mkCoAdder limit count = coiter next start
  where
    next w = CoAdderF (coAdd w) (coClear w) (coTotal w)
    start (limit, count)
```

At this point we just need to define `coAdd`, `coClear` and `coTotal`.

Clearing the current total is pretty self explanatory:
```haskell
coClear :: (Limit, Count) -> (Limit, Count)
coClear (limit, count) = (limit, 0)
```

As is querying for the current total:
```haskell
coTotal :: (Limit, Count) -> (Int, (Limit, Count))
coTotal (limit, count) = (count, (limit, count))
```

There are a few more moving parts involved when adding to the total:
```haskell
coAdd :: (Limit, Count) -> Int -> (Bool, (Limit, Count))
coAdd (limit, count) x = (test, (limit, next))
  where
    count' = count + x                        -- 1
    test = count' <= limit                    -- 2
    next = if test then count' else count     -- 3
```

At (1) we add the incoming `x` to the current total `count`, to get the (potential) new total `count'`.

At (2) we test to see if this new total is beyond our internal limit.
This is bound as `test`, which is part of what `coAdd` returns

At (3) we determine the new total.  If `count'` is beneath the `limit` then we should update the total, otherwise we leave it where it was.

Now we an interpreter that is independent from our DSL.
Just like with our DSL, we can build on things on top of our interpreter, we can destruct the `Cofree` structure in order to match it to a DSL, and we do that for different DSLs for different purposes.

## Combining the free and cofree

There is a handy way to bring `Free` and `Cofree` together, which has previously been discussed by
[Ed Kmett](http://comonad.com/reader/2008/the-cofree-comonad-and-the-expression-problem/}{http://comonad.com/reader/2008/the-cofree-comonad-and-the-expression-problem/) and
[Dan Piponi](http://blog.sigfpe.com/2014/05/cofree-meets-free.html}{http://blog.sigfpe.com/2014/05/cofree-meets-free.html).

I'll be using the terminology - and some of the instances - from Dan's post.

We start by introducing a new typeclass, `Pairing`:
```haskell
class (Functor f, Functor g) => Pairing f g where
    pair :: (a -> b -> r) -> f a -> g b -> r
```

The simplest example is with the `Identity` functor:
```haskell
instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b
```

We can step it up a notch:
```haskell
instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))
```

Although we can save some time on the second version:
```haskell
instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = pair (flip p) g f
```

So what does this buy us?  Given a `Pairing` between the underlying functors `f` and `g`, we can create a `Pairing` between `Cofree f` and `Free g`:
```haskell
instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (a :< _ ) (Pure x)  = p a x
  pair p (_ :< fs) (Free gs) = pair (pair p) fs gs
```

The pairing for our underlying functors isn't hard to do - mostly since we built the `CoAdderF` type with pairing in mind.
```haskell
instance Pairing CoAdderF AdderF where
  pair f (CoAdderF a _ _) (Add x k) = pair f (a x) k
  pair f (CoAdderF _ c _) (Clear k) = f c k
  pair f (CoAdderF _ _ t) (Total k) = pair f t k
```

Not that we're using the `Pairing` instance for `(->)` and `(,)` in the case of `Total`, and could have done something similar for `Add` if we'd tupled its arguments.

### Making use of the pairing

Now let us make use of all of this.

Given an arbitrary interpreter, we can find its limit:
```haskell
runLimit :: CoAdder a -> Int
runLimit w = pair (\_ b -> b) w findLimit
```
such that this should hold for arbitrary `x`:
```haskell
testLimit :: Int -> Bool
testLimit x = runLimit (mkCoAdder x 0) == x
```

The `Pairing` is what allows us to define our DSL and interpreter independently from one another while still being able to bring them together like this.

### Pairing the functors under DSLs and Interpreters

In general, if we have a sum type for our DSL then we'll have a product type for our interpreter.

If we have a functor for a DSL which is a sum of commands:
```haskell
data DslF k =
    Sum_1 k
  | Sum_2 k
  | ...
  | Sum_n k
```
then we'll start build the functor for an interpreter as a product of handlers like this:
```haskell
data InterpreterF k = InterpreterF {
    Product_1 k
  , Product_2 k
  , ...
  , Product_n k
  }
```
and the pairing will look like:
```haskell
instance Pairing InterpreterF DslF where
  pair f (InterpreterF j _ ... _) (Sum_1 k) = f j k
  pair f (InterpreterF _ j ... _) (Sum_2 k) = f j k
  ...
  pair f (InterpreterF _ _ ... j) (Sum_n k) = f j k
```

There is a little more going on if our commands have arguments and return results.

Say we have a functor for DSL like this:
```haskell
data DslF k =
    Sum_1 Arg_1 Arg_2 ... Arg_n ((Res_1, Res_2, ..., Res_m) -> k)
  | Sum_2 k
```

This will result in this functor for the paired interpreter
```haskell
data InterpreterF k = InterpreterF {
    Product_1 ((Arg_1, Arg_2, ..., Arg_n) -> (Res_1, Res_2, ..., Res_m, k))
  , Product_2 k
  }
```
with the following pairing:
```haskell
instance Pairing InterpreterF DslF where
  pair f (InterpreterF j _) (Sum1 a_1 a_2 ... a_n k) =
    let
      (r_1, r_2, ..., r_m, j') = j (a_1, a_2, ..., a_n)
      k'                       = k (r_1, r_2, ..., r_m)
    in
      f j' k'
  pair f (InterpreterF _ j) (Sum2 k) = f j k
```

You can do funkier things than this, but that should be enough to get you started.

## Where to from here

I have a lot I'd like to explore and write about, but the next few topics will be based on the things I already partly covered in the talk that this is based on.

The current version of our interpreter is manually handing both environment and state.
We can clean this up by using comonad transformers.
This turns out to be pretty similar to working with monad transformers, down to the fact that we can use the transformers in either `transformers` or `mtl` style.

We can break the DSL down into independent parts using coproducts, as covered in [Datatypes a la carte](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4131).
In addition to this, we can factor out the independent parts of the interpreter using products.

We can bring some other effects into the mix.
This will allows us to interact with our interpreter from the console, and to connect our DSL to our interpreter over the network.

There's a lot more in the queue, but comonad transformers, coproducts/products, and working with additional effects should make a pretty good start.
