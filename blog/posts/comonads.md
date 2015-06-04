---
title: A whirlwind tour of comonads
published: 2015-06-04 12:00:00+10:00
---

# A whirlwind tour of comonads

I recommend Gabriel Gonzalez's [explanation](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html) if you haven't read it before.

I'm posting this because

- it's part of what I covered in my talk, and I'm converting the rest of it blog form, so why not?
- some people won't read the above link, and some intuition about comonads is going to help when I start talking about comonad transformers

I'm mostly interested in conveying some intuition about comonads here, but I'll start with the typeclass and the laws.
Hopefully that will provide a kind of road map that might help when translating the things you've learned about monads into a comonadic setting.

## The typeclasses and the laws

Recall the typeclass for `Monad`:

```haskell
class Monad m where
  return    :: a -> m a
  bind      :: (a -> m b) -> m a -> m b
  join      :: m (m a) -> m a
```

You might be more used to the operator `>>=` instead of `bind`.
They're the same function with their arguments flipped.
You may also have come across `bind` in operator form as `=<<`.
I'm using `bind` here to highlight the `Monad` / `Comonad` symmetry that will make an appearance in a moment.

With monads, we're building up values in a monadic context from pure values.
I hope it is easy to see that `return` converts a pure value to a value in the monadic context directly.

We can look at `bind` as some thing that helps us build up a value in a monadic context in stages.
We're aiming for a `m b`, we've already managed to get hold of an `m a`.
We just need to provide a function to bridge between them, and that function will also build up a value in a monadic context from a pure value.

To get to `Comonad`, we flip everything.

With the usual drollness, this includes the type parameter `m`:

```haskell
class Comonad w where
  extract   :: w a -> a
  extend    :: (w a -> b) -> w a -> w b
  duplicate :: w a -> w (w a)
```

With comonads, we're tearing down values in a comonadic context to get pure values.

Again, `extract` does this directly while `extend` helps us "step down" in stages.

The laws also go through a similar transformation.

The monad laws are:
```haskell
  bind return     = id
  bind f . return = f
  bind f . bind g = bind (bind f . g)
```

and the comonad laws are:
```haskell
 extend extract      = id
 extract . extend f  = f
 extend f . extend g = extend (f . extend g)
```

There different sets of comonad laws depending on which definitions you provide in the typeclass, and they're all inter-related.
It is worth checking out the [haddocks](https://hackage.haskell.org/package/comonad/docs/Control-Comonad.html) to get a sense of those relationships.

Monads and comonads are both also functors, which we can see from these two functions:
```haskell
fmapFromMonad :: Monad m => (a -> b) -> m a -> m b
fmapFromMonad f = bind (return . f)

fmapFromComonad :: Comonad w => (a -> b) -> w a -> w b
fmapFromComonad f = extend (f . extract)
```

As an exercise for the motivated reader, can you use the `Monad` / `Comonad` laws to show that the above functions obey the `Functor` laws?

## List zippers

I think one of the better ways of conveying the intuition of comonads - at least when working with a "container" analogy - is with the List zipper.

The List zipper represents a non-empty list, with a focus on a particular element:
```haskell
data ListZipper a =
  ListZipper
    [a] -- the elements before the focus, in reverse order
     a  -- the focus
    [a] -- the elements after the focus
```

This lets us move the focus left and right efficiently:
```haskell
leftMay :: ListZipper a -> Maybe (ListZipper a)
leftMay (ListZipper [] f rs)       = Nothing
leftMay (ListZipper (l : ls) f rs) = Just $ ListZipper ls l (f : rs)

-- stay put if we're at the far left end
left :: ListZipper a -> ListZipper a
left z = maybe z leftMay z

rightMay :: ListZipper a -> Maybe (ListZipper a)
rightMay (ListZipper ls f [])       = Nothing
rightMay (ListZipper ls f (r : rs)) = Just $ ListZipper (f : ls) r rs

-- stay put if we're at the far right end
right :: ListZipper a -> ListZipper a
right z = maybe z rightMay z
```

It is pretty easy to come up with a `Functor` instance:
```haskell
instance Functor ListZipper where
  fmap g (ListZipper ls f rs) = ListZipper (fmap g ls) (g f) (fmap g rs)
```

We can define a `Comonad` instance, but you don't need to worry about the details for most of these posts:
```haskell
import Data.Maybe (catMaybes, isJust)

instance Comonad ListZipper where
  extract (ListZipper _ f _) = f
  duplicate z = ListZipper lefts z rights
    where
      gather f = tail . catMaybes . takeWhile isJust . iterate (>>= f) . Just
      lefts = gather leftMay z
      rights = gather rightMay z
```

With a suitable `Show` instance, we can see that `extract` does what we'd expect:
```haskell
let z = ListZipper [2, 1] 3 [4]

> z
| 1 | 2 > 3 < 4 |

> extract z
3
```

I'll take some license with the `Show` instance to demonstrate `duplicate`:
```
> duplicate z
||  >1<2|3|4| || |1>2<3|4| >> |1|2>3<4| << |1|2|3>4< ||
```

For `extend`, it's time to break out some graphs.

Just say we have some list zipper, `z`:

![](/images/comonad-graph1.png)

The focus is unspecified, but it's not going to change throughout these examples.

Given a function that finds the greatest value to the left of the focus:
```haskell
import Safe (maximumDef)

latch :: Ord a => Zipper a -> a
latch (Zipper l f _) =
  maximumDef f l
```
we can create a list zipper of the highest values as seen when moving from left to right through the zipper:

![](/images/comonad-graph2.png)

Given a function to determine if the focus is greater than both of its immediate neighbours:
```haskell
import Safe (headDef)

peak :: Ord a => Zipper a -> Bool
peak (Zipper l f r) =
  headDef f l < f && f > headDef f r
```
we can find all of the points which are greater than their neighbours:

![](/images/comonad-graph3.png)

Given a function to find the average value with a certain distance of the focus:
```haskell
wma ::  Int -> Zipper Double -> Double
wma n (Zipper l f r) =
  average $ take n l ++ f : take n r
```
we can find the windowed moving average of the entire list zipper:

![](/images/comonad-graph4.png)

We can also compose these functions:

![](/images/comonad-graph5.png)

## Conclusion

Aside from the fact that there's a comonad behind every zipper (which you can read more about [here (PDF)](http://www.ioc.ee/~tarmo/tsem05/uustalu0812-slides.pdf) and [here](http://stackoverflow.com/questions/25554062/zipper-comonads-generically?lq=1)), there's quite a bit more to say about the humble list zipper on it's own, but that will be the topic of a future series of posts...
