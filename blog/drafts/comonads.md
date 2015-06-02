---
title: A whirlwind tour of comonads
published: 2015-05-23 19:00:00+10:00
---

# A whirlwind tour of comonads

I'm mostly interested in conveying some intuition about comonads here, but I'll start with the typeclass and the laws, since that should provide a kind of roadmap that migth help when translating the things you've learned about monads into a comonadic setting.

Recall the typeclass for `Monad`:

```haskell
class Monad m where
  return    :: a -> m a
  (>>=)     :: m a -> (a -> m b) -> m b
  join      :: m (m a) -> m a
```

and the monad laws:

```haskell
return a >>= f === f a

m >>= return === m

(m >>= f) >>= g === m >>= (\x -> f x >>= g)
```

To get to `Comonad`, we flip everything.

With the usual drollness, this includes the type parameter `m`:

```haskell
class Comonad w where
  extract   :: w a -> a
  extend    :: w a -> (w a -> b) -> w b
  duplicate :: w a -> w (w a)
```

The laws also go through a similar transformation:

```haskell
 extend extract      = id
 extract . extend f  = f
 extend f . extend g = extend (f . extend g)
```

Monads and comonads are both also functors, which we can see from this:

```haskell
```

and this:

```haskell
```

TODO some intuition about comonads

Just say we have some list zipper, `z`:

![](/images/comonad-graph1.png)

```haskell
import Safe (maximumDef)

latch :: Ord a => Zipper a -> a
latch (Zipper l f _) =
  maximumDef f l
```

![](/images/comonad-graph2.png)

```haskell
import Safe (headDef)

peak :: Ord a => Zipper a -> Bool
peak (Zipper l f r) =
  headDef f l < f && f > headDef f r
```

![](/images/comonad-graph3.png)

```haskell
wma ::  Int -> Zipper Double -> Double
wma n (Zipper l f r) =
  average $ take n l ++ f : take n r
```

![](/images/comonad-graph4.png)

We can also compose these:

![](/images/comonad-graph5.png)

Aside from the fact that there's a comonad behind every zipper (TODO links), there's quite a bit more to say about the humble list zipper on it's own, but that will be the topic of a future post.
