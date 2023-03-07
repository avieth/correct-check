# correct-check

It's like QuickCheck or Hedgehog but with a stricter form. The definition of
a property test is factored into

- A static part which parameterizes the test, making it a contravariant functor
- A domain of unordered pseudorandom part, and complexity-ordered part, from
  which the dynamic part of the test is computed
- A test subject which computes a result from any pair of static and dynamic
  parts
- A conjunction of expectations which decide whether a triple of static,
  dynamic, and result values satisfy the property

## Testing IO with composite property tests

## Shrinking

## Unit tests
