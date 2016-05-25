The [Julia][] language has introduced into numerical computing the
useful idea of using macros to inline the evaluation, using
[Horner’s rule][], of polynomials with constant coefficients.

Because evaluating a polynomial is usually at the core of special
function implementations, the use of macros, which allow type
inference and compiling out overhead, potentially offers large gains
over doing the same evaluation with a function.

This library provides the same affordance Julia does: a single macro,
`evalpoly`, which uses [Horner’s rule][] for a real variable, and a
related rule due to Goertzel (see AOCP v2) for a complex variable.

We also go beyond what Julia offers by offering *preconditioning* of
polynomials. While Horner’s rule is optimal in respect of the total
number of operations, when the coefficients are not known in advance,
when coefficients are known in advance – and here “in advance” means
“at compile time” – then, for polynomials of the fourth degree or
greater, we can sometimes do better than Horner’s rule by
pre-computing intermediate values. (Again, see Knuth AOCP v2.)

[Julia]: http://julialang.org/
[Horner's rule]: https://en.wikipedia.org/wiki/Horner%27s_method#Description_of_the_algorithm
