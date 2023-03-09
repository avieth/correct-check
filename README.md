# correct-check

Bringing the divide between property testing and the real world.

## What we want from a testing solution

1. The usual benefits of property testing
   - _"Don't write tests, generate them!"_
   - Reproducibility of failing cases, with minimal examples
2. The ability to test IO programs

These two points seem to be at odds: if a test can do IO, then in general it
can't be reproducible, nor can it be shrunk to find a minimal failing example.
QuickCheck makes this clear in its documentation:

```Haskell
-- Warning: any random values generated inside of the argument to @ioProperty@
-- will not currently be shrunk. For best results, generate all random values
-- before calling @ioProperty@, or use 'idempotentIOProperty' if that is safe.
--
-- Note: if your property does no quantification, it will only be tested once.
-- To test it repeatedly, use 'again'.
ioProperty :: Testable prop => IO prop -> Property
```

Hedgehog, on the other hand, takes a more liberal stance, with `MonadIO`
instances for its generator, test, and property monad transformers. It even
_requires_ the use of IO to run a check:

```Haskell
data Property =
  Property {
      propertyConfig :: !PropertyConfig
    , propertyTest :: PropertyT IO ()
    }

property :: HasCallStack => PropertyT IO () -> Property

check :: MonadIO m => Property -> m Bool
```

Since allowing IO contradicts reproducibility, it's tempting to take the
approach of QuickCheck; since practical real world testing often requires
IO, it's tempting to go the route of Hedgehog. But Hedgehog's approach of
integrating the IO with shrinking generator and assertions in a monad
transformer has deal-breaking problems:

- It can't allow for continuation-passing-style IO, needed for proper
  exception handling (`bracket`) and concurrency (`withAsync`), and therefore
  can't be used to test _arbitrary_ IO programs.
- It doesn't make it easy to control what IO is done and when, since it's
  interleaved with the shrink tree of `GenT`: if it finds a failing case, then
  the shrinking phase will re-run the IO.

And so neither QuickCheck nor Hedgehog really fits as an _overall_ testing
solution for a real software project. What we end up with is a mix of property
tests for some pure functions, usually codec roundtrips, and then HUnit tests
for everything else.

This project is an attempt to find a better way.

## Reproducible tests

The foundational notion of a reproducible test looks like this:

```Haskell
data Test assertion specimen result = Test
  { subject :: specimen -> result
  , expectations :: Conjunction (Expectation assertion specimen result)
  }

data Expectation assertion specimen result = Expectation
  { assertion :: assertion
  , verification :: specimen -> result -> Bool
  }

type Conjunction = NonEmpty
```

## Search domains

A test may be applied to a domain, which defines a way to search for specimens
which cause the expectations to fail.

```Haskell
data Domain space specimen = Domain
  { search :: Strategy space
  , generate :: Gen space specimen
  }

-- Apply the test to a domain, with a given random seed, and give the first
-- counterexample found.
check :: Test assertion specimen result
      -> Domain space specimen
      -> Seed
      -> Maybe (Counterexample space specimen result assertion)

-- Everything needed to reproduce the failure, and more.
data Counterexample space specimen result assertion = Counterexample
  { seed :: Seed
  , point :: space
  , specimen :: specimen
  , result :: result
  , refuted :: NonEmpty assertion
  }
```

## Composite tests

`Test`s and `Domain` is all we need to be able to do QuickCheck-style property
testing.

```Haskell
-- Is in IO because it conjures a new random seed.
quickCheck :: Natural -- How many samples to check.
           -> Test assertion specimen result
           -> Domain space specimen
           -> IO (QuickCheck space specimen result assertion)
```

In order to express IO tests, we'll declare pure tests up-front by

- Giving them a label
- Deciding how to render their counterexamples in case of a failure
- Instructing GHC to verify that the test definition is _closed_ (see the
  section about static pointers)

These declarations can then be checked at various IO-dependent domains within
the `Composite` monad, which is essentially just `IO` and comes equipped with
a `MonadUnliftIO` instance in an effort to make it widely and easily applicable.
It looks like this:

```Haskell
example :: IO TestResult
example = composite defaultGlobalConfig $
  declare renderTestViaPretty "Some Test" (static someTest) $ \someTest ->
  compose $ do
    -- Shows that we can use arbitrary IO: even a continuation-passing-style
    -- IO like bracket. Could also use a "lifted" variant via MonadUnliftIO.
    checkPassed <- effect $ \runInIO -> bracket open closee $ \file -> runInIO $ do
      value <- readFromFile file
      -- A domain may depend upon a dynamic value like the thing we read from
      -- the file. This is okay so long as know how to render it in such a way
      -- that the generated value can be reconstructed.
      let someDomain = makeDomain value
          renderDomain = renderDomainViaPretty
      -- Check at 100 samples, in parallel (using the number of capabilities).
      check (inParallel 100) renderDomain someTest someDomain
    -- A call to check gives a Bool indicating whether it passed.
    -- A composite test may be ended early.
    unless checkPassed stop
    -- Simple, non-CPS IO can also be done in a simpler fomr
    effect_ (putStrLn "Test complete"
  where
    -- The form `static someTest` used in the declaration will ensure that
    -- this test may as well be a top-level declaration.
    someTest :: Test () (Natural, Word32) String
    someTest = ...
```

Even though `example :: IO TestResult` itself cannot be reproducible in general,
the tests checked within it _are_. The counterexamples appearing in the
`TestResult` contain enough information to be reproduced, giving meaningful
and actionable information to the investigating programmer.

```console
Found counterexample from initial seed 4bf8701495feea3f587c7849b5540001
Random seed:     4bf8701495feea3f587c7849b5540001
Search point:    1
Generated value: [3093350949]
Test result:     [3093350949,3093350949]
Refuted:         ["reverse . reverse = id" at examples/Basic.hs:47:20 in main:Basic]
[0;1mThere was 1 counterexample discovered from initial random seed [0;92;1m7064e121eb9c936b87425b101b7e31c3[0;1m[0m
[0;1m[0;91;1m✘ List reverse[0;1m[0m
  [0;1mDeclared at  [0m [0;96mexamples/Basic.hs:73:5 in main:Basic[0m
  [0;1mChecked at   [0m [0;96mexamples/Basic.hs:79:7 in main:Basic[0m
                with random seed [0;92m7ee997422298f6f187425b101b7e31c3[0m
                and search part  [0;93m1[0m
  [0;1mSpecimen     [0m [0;92m[1535][0m
  [0;1mResult       [0m [0;94m[1535, 1535][0m
  [0;1mRefuting [0m     [0;91mreverse . reverse = id[0m at [0;96mexamples/Basic.hs:47:20 in main:Basic[0m
[0;1mEnded normally[0m

[0;1mThere were 0 counterexamples discovered from initial random seed [0;92;1m4872f7b59667bc3b09c3af4c305312b1[0;1m[0m
[0;1mEnded normally[0m
```

Example output is from a deliberately wrong list reverse definition. See
[examples/Basic.hs].

## Some other interesting things

### Static pointers

This project uses [static pointers](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/static_pointers.html)
is a way they were not intended to be used. By requiring that a test be static
in order to use it in a composite, it's ensured that the test really is
reproducible. Here's an example

```Haskell
main :: IO ()
main = do
  contents <- readFileContents "./test-data.txt"
  -- This test is not reproducible: it implicitly depends upon the contents of
  -- test-data.txt
  let test = Test
        { subject = \str -> (str, lines contents)
        , expectations = that "it is a line in the file" (\(str, lns) -> str `elem` lns)
        }
  -- `static test` will be rejected because it's not a closed form.
  declare renderTestViaPretty "Lines" (static test) $ \someTest ->
    compose $ pure ()
```

Instead, the programmer must rethink the definition of the test. One way forward
would be to take the contents as part of the input, along with the string to
check. Then, the file IO can be used to define a domain, rather than a test.

### Shrinking

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

### Unit tests and simplification

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
