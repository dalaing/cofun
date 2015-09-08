---
title: Pairing and IO
published: 2015-06-13 09:00:00+10:00
---

# Recap

The [first post](/posts/free_and_cofree.html) in this series covered using free monads for DSLs, cofree comonads for interpreters, and how to pair them up.
This only covered pure DSLs and interpreters.

The [second post](/posts/monad_transformers_and_comonad_transformers.html) in this series tidied that code up by bringing monad transformers into play for the DSL and comonad transformers into play for the interpreter.
In an aside at the end of that post, I mentioned how we could update the we we do pairing to take these transformer stacks into account, and demonstrated that by adding some console `IO` into the DSL.

The [third post](/posts/coproducts_for_free_and_products_for_cofree.html) in this series covered the use of coproducts and products to separate our concerns a little further.

Now I'll be covering how to handle `IO` in the interpreter, and will also be using our adventures with `IO` to demonstrate how we can push the coproducts and products further.

# Effects in the DSL

At the start of this series, we introduced the `Pairing` typeclass:
```haskell
class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r
```
and a `Pairing` instance between `Cofree f` and `Free g`, provided that we had an instance for `Pairing f g`:
```haskell
instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (a :< _ ) (Pure x)  = p a x
  pair p (_ :< fs) (Free gs) = pair (pair p) fs gs
```

That was fine until we wanted to add monad and comonad transformers into the mix.

At that point we introduced `pairEffect`:
```haskell
pairEffect :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffect p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p (extract s) x
    Free gs -> pair (pairEffect p) (unwrap s) gs
```

With `pair`, the interaction between the free monad and the cofree comonad happens when we run into the `Pure` constructor of the `Free` type.

We may not want to wait for that to happen when we have effects in the mix - `IO` in particular - and so `pairEffect` has been written to make sure that the effects of `m` are interleaved with the interpretation of the DSL.

For a demonstration of this, we'll use `FreeT` over `IO` to interact with an interpreter via the console.

```haskell
consoleAdder' :: MonadIO m => FreeT AdderF m ()
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

consoleAdder :: MonadIO m => FreeT AdderF m ()
consoleAdder = forever consoleAdder'
```

This is nothing fancier than parsing some input to choose which action we want, printing a help message if the input didn't parse, and looping continuously via `forever`.

We can run it using `pairEffect` with our existing pure interpreter:
```haskell
run :: IO ()
run = pairEffect (\_ r -> r) (mkCoAdder 10 0) consoleAdder
```
and we'll be able to interact with it productively, despite the fact that `consoleAdder` doesn't terminate.

# Effects in the interpreter

That's good, but it is a bit unsatisfactory since we can't do any IO in the interpreter itself.

Let us change `pairEffect` so that we can do IO in the interpreter:
```haskell
pairEffect :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
pairEffect p s c = do
  a  <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p a x
    Free gs -> pair (pairEffect p) (unwrap s) gs
```

This does the effect in `CofreeT` before the effect in `FreeT`.
We can alter this function or write a new one if we want the effects to happen in the opposite order, although it seems unlikely that we'll need that.

### Altering the effects ###

We may not always want or need the same monad stack on both the DSL and interpreter sides of things.

That's easy enough to fix.

We can change our stack around on the `FreeT` side using `hoistFreeT` from `Control.Monad.Trans.Free` in the [free](https://hackage.haskell.org/package/free) package:
```haskell
hoistFreeT :: (Monad m, Functor f) => (forall a. m a -> n a) -> FreeT f m b -> FreeT f n b
```
and we can alter the stack in `CofreeT` by using `fmap`.

After that, we can use things like `lift` for simple changes, or [mmorph](http://hackage.haskell.org/package/mmorph) for more complex changes.

If we can come up with morphisms
```haskell
morphL :: Monad l => forall a. l a -> n a
```
and
```haskell
morphM :: Monad m => forall a. m a -> n a
```
we can do
```haskell
pairEffect' :: (Pairing f g, Comonad w, Monad l, Monad m, Monad n)
           => (a -> b -> r) -> CofreeT f w (l a) -> FreeT g m b -> n r
pairEffect' p s c = pairEffect p (fmap morphL s) (hoistFreeT morphM c)
```

For now, I'll assume we're using the same monad stack for our effects for the DSL and the interpeter.

### Updating the console example ###

To show off our new `pairEffect`, We'll split the console example so that the client takes care of the parsing and the interpreter takes care of printing the results.

This begins with a new `consoleAdder`:
```haskell
consoleAdder' :: MonadIO m => AdderT m ()
consoleAdder' = do
    l <- liftIO getLine
    case words l of
      ["add", x] -> void $ add (read x)
      ["clear"] -> clear
      ["total"] -> void total
      _ -> output prompt
  where
    output = liftIO . putStrLn
    prompt = unlines [
             "Commands:"
           , "  add [int]"
           , "  clear"
           ,"  total"
           ]

consoleAdder :: MonadIO m => AdderT m ()
consoleAdder = forever consoleAdder'
```
which no longer prints the results.

We then update our old interpreter:
```haskell
mkCoAdder :: Int -> Int -> CoAdder ()
mkCoAdder limit count =
    coiterT next start
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
```
so that it will print the results of the actions it carries out.

For each action, we'll use the return value to create an `IO` action which prints the return value, and we'll push it down to the next level of the cofree comonad.

We'll do this using this helper function:
```haskell
addResultLogging :: Functor f => CoAdderF (f a) -> CoAdderF (f (IO ()))
addResultLogging (CoAdderF a c t) = CoAdderF a' c' t'
  where
    a' x =
      let
        (b, k) = a x
      in
        (b, putStrLn ("add result: " ++ show b) <$ k)
    c' = return () <$ c
    t' =
      let
        (i, k) = t
      in
        (i, putStrLn ("total result: " ++ show i) <$ k)
```

We can use `addResultLogging` to make a new interpreter:
```haskell
mkCoAdderWithLogging :: Int -> Int -> CoAdder (IO ())
mkCoAdderWithLogging limit count =
    coiterT (addResultLogging <$> next) (return () <$ start)
  where
    next = CoAdderF <$> coAdd <*> coClear <*> coTotal
    start = flip StoreT count . EnvT limit . Identity $ const ()
```
which we can run with:
```haskell
run :: IO ()
run = pairEffect (\_ r -> r) (mkCoAdderWithLogging 10 0) consoleAdder
```

We now have `IO` in our DSL and in our interpreter.

There are other variants on this these, although they're mostly used when the `Pure` constructor of the `Free` monad is driving things.
Two variants I've seen around the internet that seem like they'd be pretty handy are

- working with `Cofree f (a -> b)` and `Free a`
- working with `Cofree f (a -> m b)` and `FreeT m a`

# Effects with coproducts and products

We need to work a little harder to use this with coproducts and products, but it was fun to play around with, and I think it's interesting.

For the DSL side of things, we need two things for each of the components:

- the text to print in the help message
- the parser for the input

We'll capture both of those in the `ConsoleClient` class:
```haskell
class ConsoleClient f where
  prompt :: Proxy (f ()) -> [String]
  parser :: (Monad m, CharParsing m) => m (f ())
```

For the help message data, we use a `Proxy` so that we can associate the data with our instances.
For the parser, we return a parser from the [parsers](https://hackage.haskell.org/package/parsers) package.

We can add instances of `ConsoleClient` for each of our components:

```haskell
instance ConsoleClient AddF where
  prompt _ = ["add (int)"]
  parser =
    string "add" >>
    space >>
    many digit >>= \xs ->
    return $ Add (read xs) (const ())
```

```haskell
instance ConsoleClient TotalF where
  prompt _ = ["total"]
  parser = do
    void $ string "total"
    return $ Total (const ())
```

```haskell
instance ConsoleClient ClearF where
  prompt _ = ["clear"]
  parser = do
    void $ string "clear"
    return $ Clear ()
```

and we can combine these components automatically:

```haskell
instance (ConsoleClient a, ConsoleClient b) => ConsoleClient (a :+: b) where
  prompt _ =
    prompt (Proxy :: Proxy (a ())) ++
    prompt (Proxy :: Proxy (b ()))
  parser = try (fmap InL parser) <|> fmap InR parser
```

These can be stitched together to form a much more general version of `runConsole`:
```haskell
runConsole' :: (Functor f, MonadIO m, ConsoleClient f, Monad m) => FreeT f m ()
runConsole' =
    liftIO getLine >>=
        either (\_ -> output help) liftF .
        parse parser "console parser"
  where
    output = liftIO . putStrLn
    help = unlines .
      ("Commands:" :) .
      map ("  " ++) $
      prompt (Proxy :: Proxy (f ()))

runConsole :: (Functor f, MonadIO m, ConsoleClient f, Monad m) => FreeT f m ()
runConsole = forever runConsole'
```

The main step here is to read a line, parse it, and then either lift the parsed value to our `FreeT` if the parse succeeds or print the help message if the parse fails.

This will work with any `Sum` of `ConsoleClient`s, which is nice.

Unsurprisingly, we can also abstract the changes to the interpreter on a per component basis.

To begin with, we'll generalize `addResultLogging` to a class:
```haskell
class ConsoleInterpreter f where
  addResultLogging :: Functor g => f (g a) -> f (g (IO ()))
```

We create instances for each of our components:
```haskell
instance ConsoleInterpreter CoAddF where
  addResultLogging (CoAdd f) = CoAdd (fmap (\(b, k) -> (b, putStrLn ("add result: " ++ show b) <$ k)) f)
```

```haskell
instance ConsoleInterpreter CoTotalF where
  addResultLogging (CoTotal (i, k)) = CoTotal (i, putStrLn ("total result: " ++ show i) <$ k)
```

```haskell
instance ConsoleInterpreter CoClearF where
  addResultLogging (CoClear k) = CoClear (return () <$ k)
```
and we can stitch them all together generically:
```haskell
instance (ConsoleInterpreter a, ConsoleInterpreter b) => ConsoleInterpreter (a :*: b) where
  addResultLogging (a :*: b) = addResultLogging a :*: addResultLogging b
```

We can now update `mkCoAdder` exactly as we did before:
```haskell
mkCoAdderWithLogging :: Int -> Int -> CoAdder (IO ())
mkCoAdderWithLogging limit count =
    coiterT (addResultLogging <$> next) (return () <$ start)
  where
    next = coAdd *:* coClear *:* coTotal
    start = flip StoreT count . EnvT limit . Identity . const $ ()
```

At some point it'd be nice to put together something like `reiterT`, with the goal of being able to rework a `CoAdder ()` into a `CoAdder (IO ())` more generically and without having to write a new version of `mkCoAdder`.
I'll update this post if or when I get around to it (or if someone else has a good suggestion for it).

We can run this:
```haskell
run :: IO ()
run = pairEffect' (\_ r -> r) (mkCoAdderWithLogging 10 0) (runConsole :: FreeT AdderF IO ())
```
but we need an explicit type signature for `runConsole`.

We can switch from Dan Piponi's version of `Pairing` to Ed Kmett's version at this point:
```haskell
class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r
```

Adding the `FunctionalDependencies` help drive the inference, although it looks like we may need `UndecidableInstances` to get the pairing between `:+:` and `:*:` to work.

This lets us write `run` like this:
```haskell
run :: IO ()
run = pairEffect' (\_ r -> r) (mkCoAdderWithLogging 10 0) runConsole
```
in which case the type of `runConsole` will be inferred to match the type of `mkCoAdderWithLogging`, which is pretty neat.

# Conclusion

We're starting to build up a set of handy ideas.

It's worth mentioning that this has mostly been by me wondering if I can do something, and then following the types and adapting to them to problems as I come across them.
The point is that none of this is set in stone, and there are a lot of ways these things can be sliced and diced in fun ways.
So if you're branching out and hit a roadblock with some of the pieces that I've mentioned in these posts, try to rework it (or replace it) - if there's any deep theory in play here it is almost entirely unintentional.

Next time, I'll be [pairing our DSL and interpreter over a network](/posts/pairing_over_the_network.html).

[Questions? Comments?](https://www.reddit.com/r/haskell/comments/3d0fgf/free_cofree_and_io/)
