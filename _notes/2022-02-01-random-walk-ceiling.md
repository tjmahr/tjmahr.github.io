---
title: "Random walk with a ceiling constraint"
date: 2022-02-01
tags: [r, simulation, probability]
---


I had some old code that asked what a 12-step random walk looked like if
it started at the ceiling and could on go left or stay put on the first
move. I think it was a response to someone on Twitter who claimed that a 
ceiling effect on a random walk should just produce a (half) Gaussian.


``` r
par(mar = c(4, 4, 1, 1))
set.seed(20220209)

take_one_step <- function(x, y, ceiling = 0) {
  this_step <- if (x == ceiling) {
    sample(c(-1, 0), 1)
  } else {
    sample(c(-1, 0, 1), 1)
  }
  x + this_step
}

no_ceiling <- replicate(
  30000,
  Reduce(function(x, y) take_one_step(x, y, ceiling = 1000), 0:12, init = 0)
)

yes_ceiling <- replicate(
  30000,
  Reduce(function(x, y) take_one_step(x, y, ceiling = 0), 0:12, init = 0)
)

library(patchwork)
p1 <- wrap_elements(
  full = ~ plot(table(no_ceiling), lwd = 4)
) 
p2 <- wrap_elements(
  full = ~ plot(table(yes_ceiling), lwd = 4)
)

p1 + p2
```

<div class="figure" style="text-align: center">
<img src="/figs/notes/2022-02-01-random-walk-ceiling/rw-plots-1.png" alt="center" width="80%" />
<p class="caption">center</p>
</div>

That ain't a half Gaussian. 👀
