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
static parts, each of which may be computed with IO, and such that the property
itself is known prior to any IO.

When a property within a composite test fails, it can be reproduced purely so
long as the static part it was applied to can be reconstructed. And so with this
form comes a way to conjure some order out of an IO-capable test case: even if
it does file IO, concurrency, spawns child processes, etc., it's still possible
to factor the test in such a way that a failing case will give meaningful and
actionable information to the investigating programmer.

## Unit tests

A unit test is a special case of a property test in which the domain is the
trivial search space with no randomness. But checking a property means
generating a whole bunch of random samples and searching some space at each one.
It would be wasteful to express a unit test as a property test, unless we always
knew to check it at just one seed.

This project makes an effort to help GHC to simplify a check of any property
test which is a unit test. At a high level, the idea makes good sense: checking
a unit test does not depend upon the random seed or search space point, so GHC
should be able to float it out of the `mapMaybe` which defines the search. In
case the test fails (gives `Just`), laziness takes care of this, because
searching for the existence of one failing case only requires looking at the
head of the list of failures. But if the test passes (gives `Nothing`), then
laziness doesn't help us, and a rewrite rule is required.

With enough simplifier passes, this transformation reliably works. The hope is
that users can go ahead and define unit tests as special cases of property tests
without worrying that they will waste work and slow down their test suite.

## Shrinking

TODO: discuss how shrinking works in this project.
