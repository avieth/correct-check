# correct-check

It's like QuickCheck or Hedgehog but with a stricter form. The definition of
a property test is factored into

- A static part which parameterizes the property, making it a contravariant
  functor
- A domain of unordered pseudorandom part, and complexity-ordered part, from
  which the dynamic part of the test is computed
- A test subject which computes a result from any pair of static and dynamic
  parts
- A conjunction of expectations which decide whether a triple of static,
  dynamic, and result values satisfy the property

## Testing IO with composite property tests

It's not possible to define a property that does IO. And of course not, because
we want them to be reproducible. So how do we test side-effecting programs?

The idea put forth in this project is to do it by declaring properties
up-front and then freely interleaving pure, reproducible checks of them within
arbitrary IO. The fact that properties are factored into static and dynamic
parts makes this interesting: the same property may be checked at multiple
static parts, over multiple domains, each of which may be computed with IO, but
such that the property itself is known prior to any IO.

When a property within a composite test fails, it can be reproduced purely so
long as the static part it was applied to can be reconstructed. And so with this
form comes a way to conjure some order out of an IO-capable test case: even if
it does files IO, concurrency, spawns child processes, etc., it's still possible
to factor the test in such a way that a failing case will give meaningful and
actionable information to the investigating programmer.

## Shrinking

This project takes an unorthodox approach to shrinking. It's not explicit, like
in QuickCheck, nor is it integrated, like in Hedgehog. Instead, the domain of
a property is factored into a random part, and a complexity-ordered part.

Conceptually, a domain has one unordered "axis" of all random seeds, each of
which determines some value that can be grown or shrunk by increasing or
decreasing the ordered parameter.

Generation of progressively more complicated values is carried out by picking a
random part (i.e. a 128 bit splitmix generator) and then using the search space
to increase the non-random ordered part, while keeping the random part fixed.

When a failing case is found at a given point, we can decrease the point in
complexity, while keeping the random seed fixed, until a passing case is found,
at which point we can conclude that we've found a minimal failing example.

The classic example of a list generator is not a problem here:
```Haskell
listOf :: Gen param Natural -> Gen param t -> Gen param [t]
liftOf genN genT = do
  n <- genN
  replicateM (fromIntegral n) genT
```
This general definition admits good shrinking or bad shrinking, depending on
the definition of its arguments, and how they respond to the complexity-ordered
`param`.

## Unit tests

A unit test is a special case of a property test in which the domain is the
trivial search space with no randomness. But checking a property means
generating a whole bunch of random samples and searching some space at each one.
It would be wasteful to express a unit test as a property test, unless we always
knew to check it at just one seed.

This project makes an effort to ensure that GHC will simplify a check of any
property test which is a unit test. At a high level, the idea makes good sense:
checking a unit test does not depend upon the random seed or search space point,
so GHC should be able to float it out of the `mapMaybe` which defines the search.

In case the test fails (gives `Just`), laziness takes care of this, because
searching for the existence of one failing case only requires looking at the
head of the list of failures. But if the test passes (gives `Nothing`), then
laziness doesn't help us, and a rewrite rule is required.

With enough simplifier passes, this transformation reliably works. The hope is
that users can go ahead and define unit tests as special cases of property tests
without worrying that they will waste work and slow down their test suite.
