---
title: "How to write an 'iterative' recursive function"
date: 2024-01-01
tags: [recursion, sicp, concepts]
---

I revisited *SICP* during the Christmas break. I don't know why this stuck
out to me this time---it's a chapter 1 topic, I have definitely seen
it---but SICP differentiates between two kinds of recursive functions.
Here is the example in R with factorial. First, let's make sure that
multiplication grows the stack by making it a function call instead of 
whatever kind of primitive `*` is.


``` r
`%times%` <- function(a, b) a * b

fact1 <- function(n) {
  if (n == 0) {
    message("nframe of fact1: ", sys.nframe())
    1
  } else {
    n %times% fact1(n - 1)
  }
}

fact2 <- function(n) {
  fact_iter <- function(n, accumulator) {
    if (n == 0) {
      message("nframe of fact2: ", sys.nframe())
      1 %times% accumulator
    } else {
      accumulator <- n %times% accumulator
      n <- n - 1
      fact_iter(n, accumulator)
    }
  }
  fact_iter(n, 1)
}

message("nframe of this code block: ", sys.nframe())
#> nframe of this code block: 39
fact1(10)
#> nframe of fact1: 60
#> [1] 3628800
fact2(10)
#> nframe of fact2: 51
#> [1] 3628800
```

(Note that the `sys.nframe()` values are inflated by the targets and knitr functions that are generate this notebook entry.)

The difference is that `n %times% fact(n - 1)` in `fact()` grows into a
giant single multiplication but `fact_iter(n - 1, n %times%
accumulator)` in `fact2()` does not.

The *SICP* authors [describe the difference](https://sarabander.github.io/sicp/html/1_002e2.xhtml#g_t1_002e2_002e1) between the two like so:

> Consider the first process. [...] The expansion occurs as the
> process builds up a chain of *deferred operations* (in this case, a
> chain of multiplications). [...] This type of process, characterized
> by a chain of deferred operations, is called a *recursive process*.
> Carrying out this process requires that the interpreter keep track of
> the operations to be performed later on. In the computation of $n!$,
> the length of the chain of deferred multiplications, and hence the
> amount of information needed to keep track of it, grows linearly with
> $n$ (is proportional to $n$), just like the number of steps. [...]
> 
> [In the second process, at] each step, all we need to keep track of,
> for any $n$, are the current values of the variables [`n` and
> `accumulator` in my code]. We call this *an iterative process*. In
> general, an iterative process is one whose state can be summarized by
> a fixed number of *state variables*, together with a fixed rule that
> describes how the state variables should be updated as the process
> moves from state to state and an (optional) end test that specifies
> conditions under which the process should terminate.

With something like Fibonacci, where two recursively defined things are
added together, means that the number of deferred additions grows
exponentially.
