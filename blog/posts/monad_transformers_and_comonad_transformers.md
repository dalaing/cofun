---
title: Monad transformers and comonad transformers
published: 2015-06-15 12:00:00+10:00
---

# The story so far

[Previously](/posts/free_and_cofree.html) we put together a DSL using the free monad and a corresponding interpreter using the cofree comonad.

We had an ad-hoc interpreter for our DSL:
```haskell
interpret :: Monad m => Int -> Int -> AdderT m r -> m r
interpret limit count a = do
  mr <- runFreeT a
  case mr of
    Pure r -> return r
    Free (Add x k) ->
      let
        count' = x + count
        test = count' <= limit
        next = if test then count' else count
      in
        interpret limit next (k test)
    Free (Clear k) ->
      interpret limit 0 k
    Free (Total k) ->
      interpret limit count (k count)
```

We also had an interpreter based on the cofree comonad that corresponds to the DSL:
```haskell
type CoAdder a = Cofree CoAdderF a

mkCoAdder :: Int -> Int -> CoAdder (Int, Int)
mkCoAdder limit count = coiter next start
  where
    next  = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = (limit, count)

coAdd :: (Int, Int) -> Int -> (Bool, (Int, Int))
coAdd (limit, count) x = (test, (limit, next))
  where
    count' = count + x
    test   = count' <= limit
    next   = if test then count' else count

coClear :: (Int, Int) -> (Int, Int)
coClear (limit, _) = (limit, 0)

coTotal :: (Int, Int) -> (Int, (Int, Int))
coTotal (limit, count) = (count, (limit, count))
```

Both of these work, but we can do better.
The ad-hoc interpreter can be cleaned up by using monad transformers.
It should be unsurprising - especially given the title of this post - that we can clean up the cofree comonad version of the interpreter using comonad transformers.

# A monad transformers refresher

If you're comfortable with monad transformers you can skip this.
If you're not yet comfortable with monad transformers this probably won't help much.

A few people have found these links helpful:

- [Monad Transformers Step by Step (PDF)](http://www.cs.virginia.edu/~wh5a/personal/Transformers.pdf)
- [The Greenhorn's Guide to becoming a Monad Cowboy](http://www.muitovar.com/monad/moncow.xhtml)

I'll at least put in a token effort, and some of the examples and analogies will be reused when I get to comonad transformers, so it's not all a waste.

If you've ready my [brief overview of comonads](/posts/comonads.html) you might recall my hand-wavy explanation of how monads are related to building up a value in a monadic context from a pure values.

We might need to look at the `Monad` typeclass at the correct angle to see that.

```haskell
class Monad m where
  return    :: a -> m a
  bind      :: (a -> m b) -> m a -> m b
```

It shouldn't be too much of stretch to see that `return` does this directly.
If we think of `m a` as an intermediate step on the way to building up `m b`, `bind` can be viewed as a way to use a function from a pure value to a value in a monadic context to make that step.

Throughout all of this, we are working with a single `Monad`.
We will often want to write code that deals with more than one monad at the same time.

That is where monad transformers come into play.
They "stack" monads on top of each other, via the `lift` function from the `MonadTrans` typeclass.
```haskell
class MonadTrans t where
  lift :: m a -> t m a
```

Every monad transformer is also a monad, so we're again build up a value in a monadic context.
Now we can use monadic values from lower in the "stack" to do so, by "lifting" them to the context of the monad transformer.

This will probably make more sense after some examples.

## `State` and `StateT`

The `State` monad abstracts functions that transform a state value of a given type.
It is a `newtype` wrapper around a particular form of a state transformation function:
```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

The `newtype` is used so that we can provide a `Monad` instance.
At least for me, it also helps me keep my types lining up nicely and prevents various late-night-coding-induced misbehaviours.

We use return and bind to build up more involved state transformation functions, and we can use `get` / `put` / `modify` to query and manipulate the state in these functions:
```haskell
get :: State s s
put :: s -> State s ()
modify :: (s -> s) -> State s ()
```

These functions are captured in the `MonadState s` typeclass, which has instances available in all of the places that you'd expect.

Once we've built up the state transformation function that we want, we can use `runState` / `evalState` / `execState` as our interpreters, which run the state transformation function and provide either the return value, the final state, or both.
Note that `evalState` and `execState` can both be defined in terms of `runState`.
```haskell
evalState :: State s a -> s -> a
execState :: State s a -> s -> s
```

Where `State` works for pure computations, `StateT` does the same for monadic computations for a particular monad `m`:
```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

get :: StateT s m s
put :: s -> StateT s m ()
modify :: (s -> s) -> StateT s m ()

evalStateT :: StateT s m a -> s -> m a
execStateT :: StateT s m a -> s -> m s
```

We can write all of our code in terms of `StateT`, as we can use the `Identity` monad at the bottom of our stack to finish things off.

In fact, we can define `State` in terms of `StateT` and `Identity`:
```haskell
type State s = StateT s Identity

runState :: State s -> s -> (a, s)
runState sm = runIdentity . runStateT sm
```

### An example

We used `StateT` stacked on top of our `Adder` monad in the helper function for `findLimit` in the last post:
```haskell
  -- in findLimit
  ...
  r <- execStateT findLimit' 0
  ...

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

This is a good example of using `lift` to build up the values in a monad stack and using the various domain specific functions to help interpret them.

Inside of `findLimit`, we can make use of `modify` because `StateT` is on top of the stack.

If we remember the type of `add`:
```haskell
add :: Int -> Adder Bool
```
we can see that it's not going to work in the `StateT Int Adder` stack that `findLimit'` is expecting.

However, `lift . add` has type
```haskell
lift . add :: MonadTrans t => Int -> t Adder Bool
```
or in this case
```haskell
lift . add :: Int -> StateT Int Adder Bool
```
and so everything works out.

While `findLimit'` has type
```haskell
StateT Int Adder ()
```
we can see that `execStateT findLimit' (0 :: Int)` has type
```haskell
Adder Int
```
and so `execStateT` has allowed us to temporarily make use of an additional effect.

## `Reader` and `ReaderT`

The `Reader` monad abstracts functions that operate with a value of a given type as a context or environment.
In this case it is a `newtype` wrapper around a simple function:
```haskell
newtype Reader r a = Reader { runReader :: r -> a }
```
and we use `runReader` to interpret a `Reader` monad value once we're done building it up.

The `MonadReader r` typeclass captures the `Reader` specific functions, although for this post we'll only be making us of `ask`:
```haskell
ask :: Reader r r 
```
which returns the environment value.

Just like with `State`, we can make use of `Reader` in a monad transformer stack with `ReaderT`:
```haskell
newtype ReaderT r m a = Reader { runReaderT :: r -> m a }

ask :: ReaderT r m r
```
and we can also define `Reader` in terms of `ReaderT`:
```haskell
type Reader r = ReaderT r Identity

runReader :: Reader r a -> a
runReader = runIdentity . runReaderT
```

## Cleaning up the ad-hoc interpreter

The ad-hoc interpreter is manually doing the work of a `State` monad for the count and a `Reader` monad for the limit.

Let us clean that up, using `Reader` over `State`:

```haskell
type Base m = ReaderT Int (StateT Int m)

runBase :: Monad m => Int -> Int -> Base m r -> m r
runBase limit count =
  flip evalStateT count .
  flip runReaderT limit
```

We have a helper function, `interpret'`, which builds up a value of `Base m r`.
We have written the code for `AdderT` and the `add`/`clear`/`total` helpers so that they're generic in the underlying monads.
This is where that genericity pays off - we have deferred mentioning the underlying monad right up until the point where we are interpreting our DSL, and so we can choose whatever monad we want.

```haskell
interpret' :: Monad m => AdderT (Base m) r -> Base m r
interpret' a = do
  mr <- runFreeT a
  case mr of
    Pure r -> return r
    Free (Add x k) -> do
      limit <- ask
      count <- lift get
      let count' = x + count
      let test = count' <= limit
      let next = if test then count' else count
      lift . put $ next
      interpret' (k test)
    Free (Clear k) -> do
      lift . put $ 0
      interpret' k
    Free (Total k) -> do
      count <- lift get
      interpret' (k count)
```

Since `ReaderT` is at the top of the stack in `Base`, we can use `ask` directly.

We need to use `lift` whenever we deal with the state.
This transforms computations in the `StateT Int m` monad into computations in the `ReaderT Int (StateT Int m)` monad, which is what we are working in.

We can combine both of these to get our cleaned up interpreter:
```haskell
interpret :: Monad m => Int -> Int -> AdderT (Base m) r -> m r
interpret limit count =
  runBase limit count .
  interpret'
```

At this point we're no longer explicitly threading a read-only environment value and an updatable state value through our computation and instead we access that functionality through a well-defined interface.
Things are nicer, at least in the realm of the ad-hoc version of the interpreter.

## `transformers` style and `mtl` style

There are different schools of thought on whether to use `transformers` style monad transformers - in which you have concrete types and you use explicit lifts - or `mtl` style monad transformers - in which you use typeclass constraints and don't have to lift anything.

I lean slightly towards working with `mtl` style transformers, since I like the ease with which I can use it for prototyping and putting together pieces that I might reuse - even if the reuse is just between now and the next refactoring.
Anyhow.

With `transformers` your code is explicit, and you can have multiple transformers of the same type in your stack.

With `mtl` you can't have multiple transformers of the same type, but you can easily write code that is decoupled from the concrete stack you end up using.

This only involves a change to `interpret'`:
```haskell
type Base m = ReaderT Int (StateT Int m)

runBase :: Monad m => Int -> Int -> Base m r -> m r
runBase limit count =
  flip evalStateT count .
  flip runReaderT limit

interpret' :: (MonadReader Int m, MonadState Int m) => AdderT m r -> m r
interpret' a = do
  mr <- runFreeT a
  case mr of
    Pure r -> return r
    Free (Add x k) -> do
      limit <- ask
      count <- get
      let count' = x + count
      let test = count' <= limit
      let next = if test then count' else count
      put next
      interpret' (k test)
    Free (Clear k) -> do
      put 0
      interpret' k
    Free (Total k) -> do
      count <- get
      interpret' (k count)

interpret :: (Monad m) => Int -> Int -> AdderT (Base m) r -> m r
interpret limit count =
  runBase limit count .
  interpret'
```

We are using typeclass constraints to assert that the required functionality is in the monad transformer stack.
We then use the functionality - `ask`, `get`, and `put` - directly, as it is all available from the typeclasses we mentioned in our typeclass constraints.

This means we can change the order of the transformers in the stack without having to change `interpret'`
```haskell
interpret1 :: Monad m => Int -> Int -> AdderT (ReaderT Int (StateT Int m)) r -> m r
interpret1 limit count =
  flip runStateT count .
  flip runReaderT limit .
  interpret'

interpret2 :: Monad m => Int -> Int -> AdderT (StateT Int (ReaderT Int m)) r -> m r
interpret2 limit count =
  flip runReaderT limit .
  flip runStateT count .
  interpret'
```

It also means we can factor out bits of the functionality, and limit the constraints to just what is required for what we're working on:
```haskell
clearCount :: MonadState Int m -> m ()
clearCount = put 0
```
Limiting the scope can mean that we can reuse the functionality in more places, and that our code can't make sneaky use of the other transformers in the stack.

It is worth noting that we can get these benefits from `transformers` style code if we want to, just like we can have multiple components to our `Reader` and `State` monads in `mtl` style.
There is much more to say about the two styles, and about the tools and techniques that make working with monad transformers more pleasant - including the `mmorph` library and several different parts of `lens`.
Going into further details on these is probably a post for another day.

# Comonad transformers

In contrast to monads, comonads are about converting values in a comonadic context to pure values.
I tend to think of this is "tearing down" a value in a comonadic context where monads are "building up" a value in a monadic context, however that's perhaps not the best metaphor.

```haskell
class Comonad w where
  extract   :: w a -> a
  extend    :: (w a -> b) -> w a -> w b
```

Recall that `extract` is the dual to `return` and `extend` is the dual to `bind`.
Given that, we probably won't find it too hard to view `extract` as something that converts from values in a comonad context to pure values directly, while `extend` helps us to do something similar in stages.

That pattern continues with `lower` from `ComonadTrans`:
```haskell
class ComonadTrans t where
  lower :: t w a -> w a
```

Every comonad transformer is also a comonad, and we can see that `lower` is helping us step down the stack to get closer to a pure value.

Let's look at a few of these things so we can get a bit more concrete.

## `Store` and `StoreT`

The `Store` comonad is related to the `State` monad, but does things a bit differently.

We store a function and an initial value:
```haskell
data Store s a = Store (s -> a) s
```

As an aside, if we squint at (and uncurry) `State` and `Store` we might see them as combinations of `(-> s)` and `(, s)` but in different orders.
There are volumes more to say about that, involving the relationship between adjunctions, monads, and comonads.
If you're interested to know more, you should ask in the reddit comments - there are folks in that community that are incredibly well versed in explaining those links and the various interesting avenues that branch off from there.

We can use `fmap` to modify the function via composition:
```haskell
instance Functor (Store s) where
  fmap f (Store g s) = Store (f . g) s
```

The `extract` function applies the stored function to the stored value, and `duplicate` turns the `a` into a `Store s a`:
```haskell
instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s
```

We have a number of helper functions - also accessible from the `ComonadStore s` typeclass - including:
```haskell
pos  :: Store s a -> s
seek :: s -> Store s a -> Store s a
peek :: s -> Store s a -> a
```
where

- `pos` gets the stored value
- `seek` sets the stored value, and
- `peek` applies the stored function to a new value

We'll be making use of `pos` and `seek` to maintain state in our interpreter.

In the `State` monad, `get` and `put` were able to return their values in the `State` monad, since we were building up a value in a monadic context.
In the `Store` comonad, we're tearing things down and so `pos` and `seek` look and behave like regular getter and setter functions - because that's exactly what they are.

There is a `runStore` function:
```haskell
runStore :: Store s a -> ((s -> a), s)
```
but it's not as exciting as `runState`.

In the `State` monad, we were busily building up a state transformation function, and `runState` was the interpreter that tore it down into a pure value.

In the `Store` comonad, we are doing the tearing down with the comonad, but we need something to tear down in the first place.
For that reason, where we were interested in the accessor functions inside the `newtype` wrappers for the monads, we are are more interested in the constructors for the `comonads`- since the constructors give our context a starting point.

This is also handy because we are making use of these comonad transformers in an interpreter, and that interpreter is meant to run forever.
We could use `runStore` after that, for a sense of completeness, but I'll omit it here.

As we might expect, there is a `StoreT` comonad transformer which corresponds with `Store`:
```haskell
data StoreT s w a = StoreT (w (s -> a)) a

pos  :: StoreT s w a -> s
seek :: s -> StoreT s w a -> StoreT s w a
peek :: Comonad w => s -> StoreT s w a -> a
```
and we can use the `Identity` comonad to define the `Store` in terms of `StoreT`.

## `Env` and `EnvT`

The last piece we'll need is the `Env` comonad:
```haskell
data Env e a = Env e a
```
which is similar to the `Reader` monad in its functionality.

Where `Reader` was a function from `e` to `a`, `Env` is the pair of the two values.

We have a helper function, also available in `ComonadEnv e`:
```
ask :: Env e a -> e
```
that returns the environment value, and the expected transformer version:
```haskell
data EnvT e w a = EnvT e (w a)

ask :: EnvT e w a -> e
```

## Cleaning up our interpreter

With all of that in hand, let us clean up the cofree-based interpreter.

We're adding in a transformer stack, so we'll switch from `Cofree`
```haskell
data Cofree f a = a :< f (Cofree f a)
```
to `CofreeT`
```haskell
data CofreeF f a b = a :< f b
data CofreeT f w a = CoFreeT { runCofreeT :: w (CofreeF f a (CofreeT f w a)) }
```

We're making use of `count` as a kind of state, and `limit` as a kind of environment:
```haskell
type Base a = StoreT Int (EnvT Int Identity) a
```

In the case of our interpreter, we have:
```haskell
type CoAdderT w a = CofreeT CoAdderF w a
```
and then we combine the two to get:
```haskell
type CoAdder a = CoAdderT Base a
```

Now we just need to switch to the comonad transformer version of `coiter` - called `coiterT`:
```haskell
coiterT :: (Functor f, Comonad w) => (w a -> f (w a)) -> w a -> CofreeT f w a
```
and update the value of `start`.
```haskell
mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count = coiterT next start
  where
    next  = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
```

Now `coAdd`, `coClear` and `coTotal` will change from having an argument of `(Int, Int)` to having an argument of `Base ()`.
We'll be a little more general and use `Base a` to keep the code as general as we can.

We have `StoreT` at the top of our comonad transformer stack, and `coClear` and `coTotal` only make use of the `StoreT` part of the stack.
This means we can use the `seek` and `pos` functions directly:
```haskell
coClear :: Base a -> Base a
coClear = seek 0

coTotal :: Base a -> (Int, Base a)
coTotal w = (pos w, w)
```

As `coAdd` makes use of `EnvT`, we need to use `lower` to get access to the `EnvT`:
```haskell
coAdd :: Base a -> Int -> (Bool, Base a)
coAdd w x = (test, seek next w)
  where
    count  = pos  w
    limit  = ask . lower $ w
    count' = count + x
    test   = count' <= limit
    next   = if test then count' else count
```

Party on, Wayne.

## The hard way

Things get a little trickier if we change the order of the transformer stack.

It is straight-forward to update the definition of `Base` and change the way we construct `start`:
```haskell
type Base a       = EnvT Int (StoreT Int Identity) a
type CoAdderT w a = CofreeT CoAdderF w a
type CoAdder a    = CoAdderT Base a

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next  = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = EnvT limit . flip StoreT count . Identity $ const ()
```

We need some fancy footwork to update the value in the `StoreT`, since `seek s` is defined as `peek s . duplicate`:
```haskell
coClear :: Base a -> Base a
coClear = peek 0 . lower . duplicate
```

That might not look all that fancy, but it was one the trickiest things I had to work out to put this post together.

Getting access to `pos` requires that we `lower` into the `StoreT` first:
```haskell
coTotal :: Base a -> (Int, Base a)
coTotal w = ((pos . lower $ w), w)
```
and we can use `ask` directly since `EnvT` is on the top of our stack:
```haskell
coAdd :: Base a -> Int -> (Bool, Base a)
coAdd w x = (test, peek next . lower . duplicate $ w)
  where
    count  = pos . lower $ w
    limit  = ask w
    count' = count + x
    test   = count' <= limit
    next   = if test then count' else count
```

Party on, Garth.

# From `transformers` style to `mtl` style

What we have so far demonstrates the use `transformers` style comonad transformers.
It's worth showing off the differences between that and `mtl` style comonad transformers.

We'll revert to `StoreT` over `EnvT` for this.

With `StoreT` at the top of the stack, the cases for `coClear` and `coTotal` don't change except for the type signatures:
```haskell
coClear :: ComonadStore Int w => w a -> w a
coClear = seek 0

coTotal :: ComonadStore Int w => w a -> (Int, w a)
coTotal w = (pos w, w)
```

For `coAdd` we make a similar change to the type signature.
We also drop the explicit `lower`, since the `ComonadEnv` constraint makes `ask` available to us no matter where it is in the stack:
```haskell
coAdd :: (ComonadEnv Int w, ComonadStore Int w) => w a -> Int -> (Bool, w a)
coAdd w x = (test, seek next w)
  where
    count  = pos w
    limit  = ask w
    count' = count + x
    test   = count' <= limit
    next   = if test then count' else count
```

The code for `mkCoadder` doesn't change at all.
More importantly, the `coClear`, `coTotal` and `coAdd` methods don't change even if we change the order of the transformer stack.

Both
```haskell
type Base a       = StoreT Int (EnvT Int Identity) a
type CoAdderT w a = CofreeT CoAdderF w a
type CoAdder a    = CoAdderT Base a

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next  = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
```
and
```haskell
type Base a       = EnvT Int (StoreT Int Identity) a
type CoAdderT w a = CofreeT CoAdderF w a
type CoAdder a    = CoAdderT Base a

mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next  = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = EnvT limit . flip StoreT count . Identity $ const ()
```
will work without adjustments.

# Pairing in the presence of transformers

So far we've been using the `Pairing` between `Cofree` and `Free`:
```haskell
instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (a :< _ ) (Pure x)  = p a x
  pair p (_ :< fs) (Free gs) = pair (pair p) fs gs
```
but now we're going to need something which can handle the transformer stacks.

We make use of the pure value at the root of our cofree tree with `extract`:
```haskell
extract :: CofreeT f w a -> a
```
and thanks to the `ComonadCofree` typeclass we can also access the rest of the tree with `unwrap`:
```haskell
unwrap :: Comonad w => CofreeT f w a -> f (w a)
```

These are used in `pairEffect`:
```haskell
pairEffect :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffect p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p (extract s) x
    Free gs -> pair (pairEffect p) (unwrap s) gs
```
which interleaves handling effects and pairing DSL commands with interpreter handlers.

This is handy for the cases where we will continually be producing DSL commands, and so will never get to the `Pure` case in the pairing.
We need that if we try to write a console based `Adder`:
```haskell
consoleAdder' :: MonadIO m => AdderT m ()
consoleAdder' = do
    l <- liftIO getLine
    case words l of
      ["add", x] -> add (read x) >>= \b ->
        output $ "add result: " ++ show b
      ["clear"] -> clear
      ["total"] -> total >>= \t ->
        output $ "total result: " ++ show t
      _ -> output prompt
  where
   output = liftIO . putStrLn
   prompt = unlines [
            "Commands:"
          , "  add [int]"
          , "  clear"
          , "  total"
          ]

consoleAdder :: MonadIO m => AdderT m ()
consoleAdder = forever consoleAdder'
```

We can use ` pairEffect` to couple this with our comonad transformer version of `mkCoAdder`:
```haskell
testConsole :: IO ()
testConsole = pairEffect (\_ r -> r) (mkCoAdder 10 0) consoleAdder
```
although we could just as easily have used a pure version of `mkCoAdder` stacked on top of the `Identity` comonad and `pairEffect` would have continued to work.

# Conclusion

We've now factored out the state and environment from the interpreter, but there are still aspects of both the DSL and the interpreter which are more strongly coupled than they need to be.

We'll start to deal with this in the [next post](/posts/coproducts_for_free_and_products_for_cofree.html), where we'll use coproducts to break out the orthogonal parts of the DSL and products to make a similar change to the interpreter.
After that we'll have a deeper look at making use of various effects in conjunction with free and cofree.

[Questions? Comments?](http://www.reddit.com/r/haskell/comments/39vaqj/monad_transformers_and_comonad_transformers/)
