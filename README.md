The [Julia][] language has introduced into numerical computing the
useful idea of using macros to inline the evaluation, using
[Horner’s rule][], of polynomials with constant coefficients.

Because repeatedly evaluating a polynomial is usually at the core of
any implementation of a [special function][], the use of macros, which
facilitates type inference and unrolls the loop, potentially offers
large gains over doing the same evaluation with a function.

This library provides the same affordance Julia does: a single macro,
`evalpoly`, which uses [Horner’s rule][] for a real variable, and a
related rule due to Goertzel (see AOCP v2) for a complex variable.

We also go beyond what Julia offers by offering *preconditioning* of
polynomials. While Horner’s rule is optimal in respect of the total
number of operations if the coefficients are not known in advance,
when coefficients are known in advance – and here “in advance” means
“at compile time” – then, for polynomials of the fourth degree or
greater, we can sometimes do better than Horner’s rule by
pre-computing intermediate values. (Again, see Knuth AOCP v2.)

# Examples

In *The Art of Scientific Computing*, the case for Horner’s rule is put in terms of life and death:

> We assume that you know enough never to evaluate a polynomial this way:
>
>     p=c[0]+c[1]*x+c[2]*x*x+c[3]*x*x*x+c[4]*x*x*x*x;
>
> or (even worse!),
>
>     p=c[0]+c[1]*x+c[2]*pow(x,2.0)+c[3]*pow(x,3.0)+c[4]*pow(x,4.0);
>
> Come the (computer) revolution, all persons found guilty of such criminal
> behavior will be summarily executed, and their programs won’t be! It is a matter
> of taste, however, whether to write
>
>     p=c[0]+x*(c[1]+x*(c[2]+x*(c[3]+x*c[4])));
>
> or
>
>     p=(((c[4]*x+c[3])*x+c[2])*x+c[1])*x+c[0];

The `evalpoly` macro automates this transformation. Instead of writing:

    ;; Bad
    (+ c0
       (* c1 x)
       (* c2 x x)
       (* c3 x x x)
       (* c4 x x x x))

Or manually transforming the above into

    ;; Painful
    (+ (* (+ (* (+ (* (+ (* c4 x) c3) x) c2) x) c1)
          x)
       c0)

You can use the `evalpoly` macro, which does the transformation automatically.

    ;; Good, easy, but we can do better.
    (horner:evalpoly x c0 c1 c2 c3 c4)

This is a big improvement, but we can do better. Most of the time the
coefficients of a polynomial are constants, known at compile time. In
this case, we can do better than the above by simply inlining the
constants, so the whole computation can be done without any memory
accesses.

Say you want to define the [error function][]. You look
into [Abramowitz and Stegun][A&S] and find a formula for a rational
approximation which involves a polynomial with five coefficients. We
can inline the computation of this polynomial using the `evalpoly`
macro:

    (horner:evalpoly x
      0.254829592
      -0.284496736
      1.421413741
      -1.453152027
      1.061405429)

If all this did was inline the computation, that would be a
significant gain. But it does better: because the coefficients are
known in advance, the macro is able to *precondition* the computation,
thereby saving a multiplication.

[Julia]: http://julialang.org/
[Horner’s rule]: https://en.wikipedia.org/wiki/Horner%27s_method#Description_of_the_algorithm
[special function]: https://en.wikipedia.org/wiki/Special_functions
[error function]: https://en.wikipedia.org/wiki/Error_function
[A&S]: http://people.math.sfu.ca/~cbm/aands/
