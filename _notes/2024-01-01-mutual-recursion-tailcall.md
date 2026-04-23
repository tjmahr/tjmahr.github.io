---
title: "Mutual recursion and Tailcall() in R"
date: 2024-01-01
tags: [r, recursion, language]
---

I toyed with the experimental `Tailcall()` feature in R-devel. ~~I can't
actually run this code when rendering the notebook, but~~ the basic idea
is that `sys.nframe()` shows the height of the stack when the recursion
bottoms out and it is much smaller in the `Tailcall()` examples.


``` r
is_even <- function(n) {
  if (n == 0) list(TRUE, sys.nframe()) else Tailcall(is_odd, n - 1)
}
is_odd <- function(n) {
  if (n == 0) list(FALSE, sys.nframe()) else Tailcall(is_even, n - 1)
}
is_odd(30)
#> [[1]]
#> [1] FALSE
#> 
#> [[2]]
#> [1] 40

naive_is_even <- function(n) {
  if (n == 0) list(TRUE, sys.nframe()) else naive_is_odd(n - 1)
}
naive_is_odd <- function(n) {
  if (n == 0) list(FALSE, sys.nframe()) else naive_is_even(n - 1)
}
naive_is_odd(30)
#> [[1]]
#> [1] FALSE
#> 
#> [[2]]
#> [1] 70
```

(Note that the `sys.nframe()` values are inflated by the targets and knitr functions that are generate this notebook entry.)
