We could also write a version that imposes a limit on how many times `add` is called.

TODO finish off this code
```haskell
type Base m a = ReaderT Int (StateT Int (ReaderT Int (StateT Int m))) a

interpret' :: Monad m => AdderT Base m r -> Base m r
interpret' a = do
  mr <- runFreeT a
  case mr of
   Pure r -> return r
   Free (Add x k) -> do
     limitCall <- lift . lift . ask
     countCall <- lift . lift . lift . get
     if (limitCall <= countCall)
     then
       interpret' (k False)
     else do
       limit <- ask
       count <- lift . get
       let count' = x + count
       let test   = count' <= limit
       let next   = if test then count' else count
       lift . put $ next
       interpret' (k test)
   Free (Clear k) -> do
     lift . put $ 0
     interpret' k
   Free (Total k) -> do
     count <- lift . get
     interpret' (k count)

runBase :: Monad m => Int -> Int Int -> Int -> Base m r -> m r
runBase limitValue countValue limitCall countCall =
  flip runStateT countCall .
  flip runReaderT limitCall .
  flip runStateT countValue .
  flip runReaderT limitValue

interpret :: Monad m => Int -> Int -> Int -> Int -> AdderT Base m r -> m r
interpret limitValue countValue limitCall countCall =
  runBase limitValue countValue limitCall countCall .
  interpret'
```

With `mtl` you can't have multiple transformers of the same type, but you can write code that is decoupled from the concrete stack you end up using.

This is what it looks like:
```haskell
interpret' :: (MonadReader Int m, MonadState Int m) => AdderT m r -> m r
interpret' a = do
  mr <- runFreeT a
  case mr of
   Pure r -> return r
   Free (Add x k) -> do
     limit <- ask
     count <- get
     let count' = x + count
     let test   = count' <= limit
     let next   = if test then count' else count
     put next
     interpret' (k test)
   Free (Clear k) -> do
     put 0
     interpret' k
   Free (Total k) -> do
     count <- get
     interpret' (k count)
```
We're using typeclass constraints to assert that the required functionality is in the monad transformer stack.
We then use the functionality - `ask`, `get`, and `put` - directly.

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

You can reap the same benefits from either style of monad transformer code by doing a little more work.

You can get `mtl` style reuse of of `transfomers` style code by using either the `mmorph` TODO link library or `zoom` and `magnify` from `lens` TODO links to TODO

You can have multiple components in your `mtl` style `State` and `Reader` stacks by using classy lenses.

They're all worth coming up to speed with, but going into further details on these is probably a post for another day.
