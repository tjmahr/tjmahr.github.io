---
title: "Simulation for a probability paradox"
date: 2024-01-01
tags: [probability, simulation, paradox]
---



This paradox is a flavor of the [Sleeping
Beauty](https://en.wikipedia.org/wiki/Sleeping_Beauty_problem) paradox
or the [Boy or Girl](https://en.wikipedia.org/wiki/Boy_or_girl_paradox)
paradox.

> You are given an urn containing 100 balls; n of them are red, and 100-n
> are green, where n is chosen uniformly at random in [0, 100]. You take
> a random ball out of the urn—it’s red—and discard it. The next ball you
> pick (out of the 99 remaining) is: [poll omitted]
[[source]](https://twitter.com/littmath/status/1751648838501224790)

And here is some code I wrote to run a simulation to get the unexpected
solution.


``` r
f <- function(n = 100) {
  n_red <- sample.int(n, 1)
  p1 <- n_red / n
  p2 <- (n_red - 1) / (n - 1)
  draw <- sample(c("r", "g"), 1, prob = c(p1, 1 - p1))
  if (draw == "g") {
    "skip"
  } else {
    sample(c("r", "g"), 1, prob = c(p2, 1 - p2))
  }
}
counts <- replicate(100000, f()) |> 
  table()
counts
#> 
#>     g     r  skip 
#> 16549 33787 49664

# p(color | first red)
counts[1:2] |> proportions()
#> 
#>         g         r 
#> 0.3287707 0.6712293
```
