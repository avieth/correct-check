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
