---
title: "Log-likelihood functions"
date: 2025-01-01
tags: [r, statistics, modeling]
---

These two ways of computing the log-likelihood (an automatic way and
naive way) differ because `sigma(m)` estimates sigma so it applies a (1
/ (N - p)) correction but `logLik()` does not.


``` r
m <- lm(mpg ~ wt + disp, mtcars)
logLik(m)
#> 'log Lik.' -78.08389 (df=4)

ls <- dnorm(mtcars$mpg, predict(m), sigma(m))
sum(log(ls))
#> [1] -78.15893
```


