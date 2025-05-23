# accelerate-tests

More tests for [Accelerate](https://github.com/AccelerateHS/accelerate). This
repository contains tests that are not suitable, one way or another, to be put
in the main Accelerate test suite. Generally, this is because one of the
following is true:

- The test tries to reproduce a rare, nondeterministic issue, and hence must be
  run a very large number of times to give any useful information. This should
  be done when there is a specific reason for it, not in normal testing.
- The tests are large programs from other sources and/or authors.

We may decide to move some of these to the main test suite anyway. We may also
not.
