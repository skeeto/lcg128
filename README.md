# Truncated 128-bit LCG bignum PRNG for Emacs Lisp

This package provides a seedable, high quality PRNG built upon
bignums. The algorithm is a [minimal standard 128-bit Linear
Congruential Generator (LCG)][pcg] with [parameters from Emacs
Calc][lcg]:

    m = 0x86cbe851ccd8e971cdd864f4f5fd99b5
    a = 0xc3385d20aa58ba6d70f12e993960a383
    state = (state*m + a) % (1 << 128)
    return state >> 64

Since it uses bignums, `lcg128` requires Emacs 27 or later.

The `random` built-in function is very fast, but it doesn't use any
particular algorithm. The algorithm varies by platform and build
configuration, and all options have poor statistical quality. It's
seedable, but since the algorithm could be anything, that doesn't mean
much. The state is global and inaccessible.

The `cl-random` generator also does not use a specific algorithm, but
the algorithm has been the same for a long time: a lagged Fibonacci
generator (LCG). It's around 2x to 6x faster than `lcg128`. The
statistical quality isn't as bad as `random`, but still poor. The API
is modeled closely after Common Lisp and deliberately omits seeding.
The state is an opaque object, and historically it doesn't support
serialization (the format has changed). **The `cl-random` function
itself does not support bignums nor any output wider than 31 bits.**

This package, `lcg128`, has a specific algorithm documented above. The
state is a cons cell containing one 128-bit integer, and will never
change, meaning the state can be serialized. It's also much smaller
than a `cl-random` state. It may be seeded using any lisp object. The
statistical quality is excellent for a non-cryptographic PRNG. Unlike
`cl-random`, the `lcg128-range` function supports bignums.

| generator  | speed     | quality   | state    | seedable |
|------------|-----------|-----------|----------|----------|
| random     | very fast | very poor | hidden   | partial  |
| cl-random  | slow      | poor      | opaque   | no       |
| lcg128     | slowest   | good      | explicit | yes      |

## Benchmarks

Note: The benchmark was run [with this `cl-random` patch][patch].

    $ make bench
    lcg128    [small] (1.782784913 130 0.646757037)
    cl-random [small] (0.492068786 0 0.0)
    random    [small] (0.06470748800000001 0 0.0)
    lcg128    [large] (1.777883544 130 0.6488473080000001)
    cl-random [large] (1.388324855 0 0.0)
    random    [large] (0.057713930000000004 0 0.0)

## API

```el
(defun lcg128-create (&optional seed)
(defun lcg128-p (object)
(defun lcg128-copy (state)
(defun lcg128-uniform (&optional state))
(defun lcg128-bits (nbits &optional state))
(defun lcg128-range (limit &optional state))
(defun lcg128-shuffle (seq &optional state))
(defun lcg128-normal (&optional state))
```


[lcg]: https://nullprogram.com/blog/2019/11/19/
[patch]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=38753
[pcg]: http://www.pcg-random.org/posts/does-it-beat-the-minimal-standard.html
[lcg]: https://en.wikipedia.org/wiki/Lagged_Fibonacci_generator
