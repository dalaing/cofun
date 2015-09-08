---
title: Pairing over the network
published: 2015-09-07 12:00:00+10:00
---

# The goal

This is part of an ongoing series of posts about [fun with Cofree Comonads](../).
If you're just jumping in, I'd recommend reading the series from the start, since this post assumes that you're up to date.

For this post I want to focus on establishing a pairing between a `Free`-based DSL with a `Cofree`-based interpreter over the network.

This is all meant to be illustrative.
I won't be spending any time on getting the asynchronous exceptions or the binary streaming to anything beyond a toy level, although I may double back at a later date and take care of that.

Coproducts and products will take a back seat for a post or two, although they'll return as soon as we have described all of the pieces we need.

# Two approaches

Recall that we had a type `AdderF`:
```haskell
data AdderF k = Add Int (Bool -> k)
              | Clear k
              | Total (Int -> k)
```
and a type `CoAdderF`:
```haskell
data CoAdderF k = CoAdderF {
    addH   :: Int -> (Bool, k)
  , clearH :: k
  , totalH :: (Int, k)
  }
```

In earlier posts, we have discussed that we can view each of the constructors `Add`, `Clear` and `Total` as being comprised of a set of input parameters and a function from output parameters to `k`.

If we view our `Free AdderF` as a protocol, we can view these as requests and responses.
Considering our goal is to establish a pairing over the network, this seems like a good avenue to explore.

To formalize the protocol / request / response notion somewhat, if we had
```haskell
type Client req res k = (req, res -> k)
```
we could recast `AdderF` as
```haskell
data AdderF k = Add   (Client Int Bool k)
              | Clear (Client () () k)
              | Total (Client () Int k)
```
and with
```haskell
type Interpreter req res k = req -> (res, k)
```
we could recast `CoAdderF` as
```haskell
data CoAdderF k = CoAdderF {
    addH   :: Interpreter Int Bool k
  , clearH :: Interpreter () () k
  , totalH :: Interpreter () Int k
  }
```

We can turn this "inside out" if we define
```haskell
data AdderReq = AddReq Int
              | ClearReq
              | TotalReq
```
and
```haskell
data AdderRes = AddRes Bool
              | ClearRes
              | TotalRes Int
```
from which we can get
```haskell
type AdderF k = Client AdderReq AdderRes k
```
and
```haskell
type CoAdderF k = Interpreter AdderReq AdderRes k
```

Regardless of which approach we take, we're going to need:

- pairings between the DSL and the interpreter
- serialization of our requests and responses
- the ability to handle errors and IO

The first approach is going to require a serialization strategy where none of the coproducts contributing to `AdderF` will get in each others way.
We'll return to that once we've covered some of the associated type-level machinery in the next post.

The second approach has some risk of mismatching requests and responses while they are in flight.
Since we need to handle errors anyway, we just need to make sure that our methods of error handling is sufficient to deal with mismatched requests and responses and all should be well.

# The intermediate stage

Since we're going to handle effects when we take to the network, lets weave some effects into our client and interpreter types:
```haskell
data NetworkClientF req res m k = NetworkClientF (req, res -> m k)

data NetworkInterpreterF req res m k = NetworkInterpreterF (req -> m (res, k))
```

We also want these to be `Functor`s, so that we can pair them:
```haskell
instance Functor m => Functor (NetworkClientF req res m)  where
  fmap f (NetworkClientF k) = NetworkClientF (fmap (fmap (fmap f)) k)

instance Functor m => Functor (NetworkInterpreterF req res m) where
  fmap f (NetworkInterpreterF k) = NetworkInterpreterF (fmap (fmap (fmap f)) k)
```

The problem we have now is that `Pairing` doesn't know about effects:
```haskell
class Pairing f g | f -> g, g -> f where
  pair :: (a -> b -> r) -> f a -> g b -> r
```

We deal with this in the usual manner - if we need effects, add an `m` somewhere:
```haskell
class PairingM f g m | f -> g, g -> f where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r
```
(although this was actually concocted in a slightly less ad-hoc manner than just adding an `m` and hoping).

Now we have what we need to link the client and interpreter:
```haskell
instance Monad m => PairingM (NetworkInterpreterF req res m) (NetworkClientF req res m) m where
  pairM p (NetworkInterpreterF fi) (NetworkClientF (rq, fc)) = do
    (rs, ki) <- fi rq
    kc <- fc rs
    p ki kc
```
given some way to actually make use of the `PairingM`:
```haskell
pairEffectM :: ( Functor (f m)
               , PairingM (f m) (g m) m
               , Comonad w
               , Monad m
               )
            => (a -> b -> m r) -> CofreeT (f m) w (m a) -> FreeT (g m) m b -> m r
pairEffectM p s c = do
  a <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> p a x
    Free gs -> pairM (pairEffectM p) (unwrap s) gs
```

This is where we need to get to as a first step to connecting our DSL and interpreter over the network.

# Getting from the beginning to the middle

Our previous console example was
```haskell
run :: IO ()
run = pairEffect (\_ r -> r) (mkCoAdderWithLogging 10 0) runConsole
```
which connects an interactive console with an interpreter which prints information about what it is up to.

This made use of
```haskell
runConsole :: (Functor f, MonadIO m, ConsoleClient f, Monad m) => FreeT f m ()
```
and the instance of `ConsoleClient` for `AdderF`, along with
```haskell
mkCoAdderWithLogging :: MonadIO m => Int -> Int -> CofreeT CoAdderF (StoreT Int (EnvT Int Identity)) (m ())
```

Our goal is to get the same functionality working across the network.

What we have so far has the form `FreeT AdderF m a` or `CofreeT CoAdderF w a`, and we need to get it into a form that uses `NetworkClientF` or `NetworkInterpreterF`.

There are some useful helper functions for working with `FreeT` in the `Control.Monad.Trans.Free` module of the `free` package.

Amongst them are
```haskell
transFreeT :: (Monad m, Functor g) => (forall a. f a -> g a) -> FreeT f m b -> FreeT g m b 
```
which lets us swap out the underlying `Functor`, and 
```haskell
hoistFreeT :: (Monad m, Functor f) => (forall a. m a -> n a) -> FreeT f m b -> FreeT f n b
```
which lets us swap out (or transform) the underlying `Monad`.

There are also some very handy tools we can use in conjunction with `hoistFreeT` in the [mmorph](http://hackage.haskell.org/package/mmorph) package.

For cofree, there are instances of `ComonadHoist` throughout the `free` package which will give us hoisting, and the `master` branch on github has
```haskell
transCofreeT :: (Comonad w, Functor g) => (forall a. f a -> g a) -> CofreeT f w b -> CofreeT g w b 
```
if you're feeling adventurous and want to play along.

As an aside: ordinarily I'd try to use `transFreeT` and `hoistFreeT` to try to get by with a regular `Pairing` instance rather than a `PairingM` instance.
I tried a few variants to make that work, but struggled with some of the details of getting the error handling interleaved with the pairing in the correct fashion.
I'll be trying again later on, but if anyone happens to play around with this and finds a way to do without `PairingM`, please get in touch and let me know.

We're usually working with `FreeT AdderF m a` and we'd like to get that to `FreeT (NetworkClientF AdderReq AdderRes m) m a`, so it seems like `transFreeT` is just what we want.
Now we need a function to transform `AdderF k` into `NetworkClientF AdderReq AdderRes m k` and a function to transform `CoAdderF k` into `NetworkInterpreterF AdderReq AdderRes m k`.

With that in mind - and realizing that we may want to use some of this machinery for more than this single toy example - we set up the type families:
```haskell
class ToNetworkClient (a :: * -> *) m where
  type ClientReq a
  type ClientRes a
  toNetworkClient :: a k -> NetworkClientF (ClientReq a) (ClientRes a) m k

class ToNetworkInterpreter (a :: * -> *) m where
  type InterpreterReq a
  type InterpreterRes a
  toNetworkInterpreter :: a k -> NetworkInterpreterF (InterpreterReq a) (InterpreterRes a) m k
```
to assist us.

The translation for `AdderF` makes use of our network error type:
```haskell
data NetError = Disconnected
              | UnexpectedRequest
              | UnexpectedResponse
```
and introduces a `MonadError NetError m` constraint from the `Control.Monad.Except` module in the `mtl` package:
```haskell
instance (Monad m, MonadError NetError m) => ToNetworkClient AdderF m where
  type ClientReq AdderF = AdderReq
  type ClientRes AdderF = AdderRes

  toNetworkClient (Add x f) = NetworkClientF (AddReq x, g)
    where
      g (AddRes b) = return $ f b
      g _ = throwError UnexpectedResponse

  toNetworkClient (Clear k) = NetworkClientF (ClearReq, g)
    where
      g ClearRes = return k
      g _ = throwError UnexpectedResponse

  toNetworkClient (Total f) = NetworkClientF (TotalReq, g)
    where
      g (TotalRes i) = return $ f i
      g _ = throwError UnexpectedResponse
```

The translation for `CoAdderF` involves less drama:
```haskell
instance Monad m => ToNetworkInterpreter CoAdderF m where
  type InterpreterReq CoAdderF = AdderReq
  type InterpreterRes CoAdderF = AdderRes

  toNetworkInterpreter (CoAdderF a c t) = NetworkInterpreterF $ \rq -> case rq of
      AddReq i -> let
                    (b, k) = a i
                  in return (AddRes b, k)
      ClearReq -> return (ClearRes, c)
      TotalReq -> let
                    (i, k) = t
                  in return (TotalRes i, k)
```

At this point we can recast our DSLs and interpreters into a more network-friendly form, but we still need to connect them up to the network.

# Getting from the middle to the end

## The client side

From the client side, we want to translate our `FreeT c m a` to a `FreeT (NetworkClientF (ClientReq c) (ClientRes c) m) m a`, and then pair it with something to connect it with the network.

We want something like this:
```haskell
pairClient :: ( Functor m
              , MonadError NetError m
              , ToNetworkClient c m
              )
           => FreeT c m () -> m ()
pairClient = pairEffectM (\_ r -> return r) ??? . transFreeT toNetworkClient
```
and from the pairing we already have, we can reasonably assume that `???` is going to be some kind of `CofreeT (NetworkInterpreterF (InterpreterReq c) (InterpreterRes c) m) w a`

Making use of the `network-simple` package and assuming that we've already got access to a connected `Socket`, we can devise something like that:
```haskell
mkClientConnector :: ( Functor m
                     , MonadReader Socket m
                     , MonadError NetError m
                     , MonadIO m
                     , Binary req
                     , Binary res
                     )
                  => Cofree (NetworkInterpreterF req res m) (m ())
mkClientConnector = coiterT f (Identity (return ()))
  where
    f w = NetworkInterpreterF $ \req -> do
      -- get the socket
      s <- ask
      -- encode and send the request
      send s . L.toStrict . encode $ req
      -- read and decode the response
      res <- fmap (decode . L.fromStrict) <$> recv s 1024
      -- check for disconnection
      case res of
        Nothing -> throwError Disconnected
        Just x -> return (x, w)
```

This is where we end up needing `Binary` instances for `AdderReq` and `AdderRes`:
```haskell
instance Binary AdderReq where
  put (AddReq i) = putWord8 0 >> put i
  put ClearReq = putWord8 1
  put TotalReq = putWord8 2

  get = do
    x <- getWord8
    case x of
      0 -> AddReq <$> get
      1 -> return ClearReq
      2 -> return TotalReq
      _ -> empty

instance Binary AdderRes where
  put (AddRes b) = putWord8 0 >> put b
  put ClearRes = putWord8 1
  put (TotalRes i) = putWord8 2 >> put i

  get = do
    x <- getWord8
    case x of
      0 -> AddRes <$> get
      1 -> return ClearRes
      2 -> TotalRes <$> get
      _ -> empty
```

With that in hand, we can update `pairClient` to make use of `mkClientConnector`:
```haskell
pairClient :: ( Functor m
              , MonadReader Socket m
              , MonadError NetError m
              , MonadIO m
              , ToNetworkClient c m
              , Binary (ClientReq c)
              , Binary (ClientRes c)
              )
           => FreeT c m () -> m ()
pairClient = pairEffectM (\_ r -> return r) mkClientConnector . transFreeT toNetworkClient
```

We add a helper function to connect the socket and handle the `MonadReader Socket` and `MonadError NetError` obligations:
```haskell
runClient :: (MonadIO m, MonadMask m) => HostName -> ServiceName -> ReaderT Socket (ExceptT NetError m) a -> m (Either NetError a)
runClient host service x = connect host service $ \(sock, _) -> runExceptT . flip runReaderT sock $ x
```

With all of this in hand, we can finally connect our `runConsole` function to the network:
```haskell
networkClient :: HostName -> ServiceName -> IO (Either NetError ())
networkClient host service = runClient host service (pairClient console)
  where
    console = runConsole :: FreeT AdderF (ReaderT Socket (ExceptT NetError IO)) ()
```

## The interpreter side

We end up with similar pieces of functionality to get the interpreter connected to the network.

We have the pairing function:
```haskell
pairInterpreter :: ( Functor m
                   , Comonad w
                   , MonadReader Socket m
                   , MonadError NetError m
                   , MonadIO m
                   , ToNetworkInterpreter i m
                   , Binary (InterpreterReq i)
                   , Binary (InterpreterRes i)
                   )
                => CofreeT i w (m ()) -> m ()
pairInterpreter server = pairEffectM (\_ r -> return r) (transCofreeT toNetworkInterpreter server) mkInterpreterConnector
```
and we have the `FreeT (NetworkClientF (ClientReq i) (ClientRes i) m) ()` that the pairing function needs:
```haskell
mkInterpreterConnector :: ( Functor m
                          , MonadReader Socket m
                          , MonadError NetError m
                          , MonadIO m
                          , Binary req
                          , Binary res
                          )
                       => FreeT (NetworkClientF req res m) m ()
mkInterpreterConnector = do
  -- get the socket
  s <- ask
  -- read and decode the request 
  r <- fmap (decode . L.fromStrict) <$> recv s 1024
  case r of
    -- stop on disconnection
    Nothing -> FreeT $ throwError Disconnected
    Just x -> FreeT . return . Free . NetworkClientF $ (x, \t -> do
      -- encode and send the response
      _ <- liftIO . send s . L.toStrict . encode $ t
      -- keep on going
      return mkInterpreterConnector
    )
```

With the client we didn't need to use recursion, since the `coiterT` in `mkClientConnector` gave us that.
Here we need to make a recursive call, but we don't make the call in the event of a disconnection.

We have a helper function to serve the interpreter on a particular address and port:
```haskell
runInterpreter :: HostName -> ServiceName -> ReaderT Socket (ExceptT NetError IO) () -> IO ()
runInterpreter host service x =
  serve (Host host) service $ \(sock, _) ->
     void . runExceptT . flip runReaderT sock $ x
```
with which we can connect our `CoAdderF` to the network:
```haskell
networkInterpreter :: HostName -> ServiceName -> IO ()
networkInterpreter host service = runInterpreter host service (pairInterpreter $ mkCoAdderWithLogging 10 0)
```

# Conclusion

There were a few pieces in here, but we should be able to get some good usage out of them in the future.

Next we will revisit the products and sums of functors (and the pairings between them).
I figured out a bit of this on my own, but the [excellent course notes by Andres Löh](https://github.com/kosmikus/SSGEP/blob/master/LectureNotes.pdf) got me the rest of the way there.

After that we will be looking at how to pair our coproduct-based DSLs and product-based interpreters over the network.

Since we have the various actions in serialized form, we can make further use of that.
This could involve playing around with event sourcing - which has a lot of fun side-alleys associated with it - or it could involve setting up a write-ahead log in an attempt to make the pairing-over-the-network atomic an durable.

I've got a growing queue of `Cofree`-based topics I want to write about, so some of those things might come later.

[Questions? Comments?](https://www.reddit.com/r/haskell/comments/3k1ch7/pairing_over_the_network/)
